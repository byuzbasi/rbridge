// Parallelism is confined to CV folds; BLAS threads are controlled by the caller.
#define ARMA_DONT_USE_OPENMP
#include <RcppArmadillo.h>
#include <stdexcept>
#include <vector>
#ifdef _OPENMP
#include <omp.h>
#endif
using namespace Rcpp;
using namespace arma;
using namespace std;
// [[Rcpp::depends(RcppArmadillo)]]



// [[Rcpp::export]]
List standard(const arma::mat & x){
  // standardize a matrix
  List output;
  arma::uword n = x.n_rows;
  arma::uword p = x.n_cols;
  double er = 0;
  arma::mat xx  = zeros(n, p);
  arma::vec c = zeros(p, 1);
  arma::vec s = zeros(p, 1); 
  
  
  for (arma::uword j = 0; j < p; j++){
    //Center
    c[j] = sum(x.col(j))/n;
    xx.col(j) = x.col(j) - c[j];
    er = dot(xx.col(j),xx.col(j))/n;
    s[j] = sqrt(er);
    xx.col(j) /= s[j];
  }
  output["xx"] = xx;
  output["c"] = c;
  output["s"] = s;
  return(output);
}



// Preserve the historical contiguous folds without repeated searches/deletions.
arma::uvec TrainingRows(arma::uword n, arma::uword first, arma::uword end) {
  arma::uvec train(n - (end - first));
  arma::uword j = 0;
  for (arma::uword i = 0; i < n; ++i) {
    if (i < first || i >= end) train(j++) = i;
  }
  return train;
}

arma::mat CheckedSolve(const arma::mat & A, const arma::mat & B,
                       const char * context) {
  arma::mat solution;
  // Never silently replace an unidentified system with an SVD approximation.
  if (!arma::solve(solution, A, B, arma::solve_opts::no_approx) ||
      !solution.is_finite()) {
    throw std::runtime_error(std::string(context) +
      ": singular or numerically unresolved system; check lambda and restrictions");
  }
  return solution;
}

// Internal build information, surfaced through rbridge_info().
// [[Rcpp::export]]
List Engine_Info() {
#ifdef _OPENMP
  return List::create(_["openmp"] = true, _["openmp_version"] = _OPENMP);
#else
  return List::create(_["openmp"] = false, _["openmp_version"] = 0);
#endif
}


// [[Rcpp::export]]
arma::mat BetaIntial(arma::mat x, 
                     arma::colvec y, 
                     arma::colvec lambda) {
  arma::uword n_lam = lambda.n_elem;
  arma::mat U,V;
  arma::vec s;
  svd_econ(U,s,V,x);
  
  arma::mat rhs = trans(U)*y;
  arma::uword sx = s.n_elem;
  arma::vec div = repmat(pow(s,2),n_lam,1)+repelem(lambda, sx, 1);
  arma::vec a = repmat(s%rhs,n_lam,1)/div;
  mat A;
  A.insert_cols(0, a);
  A.reshape(sx, n_lam);
  arma::mat coef = V*A; 
  return(coef);
}



// [[Rcpp::export]]
arma::vec Lambdas_Grid(const arma::mat & x,
                       const arma::vec & y,
                       const double & q,
                       const double & lambda_min,
                       const arma::uword & n_lambda){
  arma::uword n = x.n_cols;
  arma::vec lambda = zeros(n_lambda, 1);
  double alpha = 0;
  
  if (q > 1){
    alpha = 0.001;
  } else {
    alpha = 0.05;
  }
  
  arma::mat x_std = x;
  arma::vec y_std = y;
  
  double lambda_max = (1/ alpha) * max(abs(y_std.t() * x_std)) / n;
  lambda = exp(linspace(log(lambda_max), log(lambda_min), n_lambda));
  
  return(lambda);
}


// [[Rcpp::export]]
arma::sp_mat Bridge(const arma::mat & x,
                    const arma::colvec & y,
                    const double & q,
                    const arma::colvec & lambda,
                    const double & converge,
                    const double & eta) {
  const arma::uword p = x.n_cols;
  arma::sp_mat beta_mat(p, lambda.n_elem);
  const arma::mat beta_start = BetaIntial(x, y, lambda);
  const arma::vec x_ty = trans(x) * y;
  // Build the full Gram matrix only if a full active set is encountered.
  // A path starting with fewer predictors does not allocate an unused p x p cache.
  arma::mat full_gram;
  bool have_full_gram = false;

  for (arma::uword i = 0; i < lambda.n_elem; ++i) {
    arma::vec beta_prev = beta_start.col(i);
    double change = converge;
    arma::uword iteration = 0;
    arma::uvec cached_active;
    arma::mat active_gram;
    while ((change > eta) & (iteration <= 100)) {
      ++iteration;
      const arma::uvec del = find(abs(beta_prev) < eta);
      if (del.n_elem == p) {
        beta_prev.rows(del).zeros();
        change = 0;
      } else if (del.n_elem > 0) {
        beta_prev.rows(del).zeros();
        const arma::uvec active = find(abs(beta_prev) >= eta);
        if (cached_active.n_elem != active.n_elem ||
            !arma::all(cached_active == active)) {
          if (have_full_gram) {
            active_gram = full_gram.submat(active, active);
          } else {
            const arma::mat x_active = x.cols(active);
            active_gram = trans(x_active) * x_active;
          }
          cached_active = active;
        }
        arma::mat system = active_gram;
        system.diag() += lambda(i) * q *
          pow(abs(beta_prev.elem(active)), q - 2) / 2;
        arma::vec beta_curr = beta_prev;
        beta_curr.elem(active) = CheckedSolve(system, x_ty.elem(active), "bridge");
        change = accu(square(beta_curr - beta_prev));
        beta_prev = beta_curr;
      } else {
        if (!have_full_gram) {
          full_gram = trans(x) * x;
          have_full_gram = true;
        }
        arma::mat system = full_gram;
        system.diag() += lambda(i) * q * pow(abs(beta_prev), q - 2) / 2;
        const arma::vec beta_curr = CheckedSolve(system, x_ty, "bridge");
        change = accu(square(beta_curr - beta_prev));
        beta_prev = beta_curr;
      }
    }
    beta_mat.col(i) = beta_prev;
  }
  return beta_mat;
}

// [[Rcpp::export]]
arma::mat RBridge(const arma::mat & x,
                 const arma::colvec & y,
                 const double & q,
                 const arma::colvec & lambda,
                 const arma::mat & R,
                 const arma::mat & r,
                 const double & converge,
                 const double & eta) {
  const arma::uword p = x.n_cols;
  arma::mat beta_mat(p, lambda.n_elem, fill::zeros);
  const arma::mat beta_start = BetaIntial(x, y, lambda);
  const arma::mat gram = trans(x) * x;
  // One factorization per iteration solves both the bridge and restriction RHS.
  const arma::mat rhs = join_rows(trans(x) * y, trans(R));
  for (arma::uword i = 0; i < lambda.n_elem; ++i) {
    arma::vec beta_prev = beta_start.col(i);
    double change = converge;
    arma::uword iteration = 0;
    while ((change > eta) & (iteration <= 100)) {
      ++iteration;
      const arma::uvec del = find(abs(beta_prev) < eta);
      if (del.n_elem == p) {
        beta_prev.rows(del).zeros();
        change = 0;
      } else {
        if (del.n_elem > 0) beta_prev.rows(del).fill(eta);
        arma::mat system = gram;
        system.diag() += lambda(i) * q * pow(abs(beta_prev), q - 2) / 2;
        const arma::mat solutions = CheckedSolve(system, rhs, "rbridge");
        const arma::vec bridge_beta = solutions.col(0);
        const arma::mat projected = solutions.cols(1, R.n_rows);
        const arma::vec correction = CheckedSolve(R * projected,
          R * bridge_beta - r, "rbridge restriction");
        const arma::vec beta_curr = bridge_beta - projected * correction;
        change = accu(square(beta_curr - beta_prev));
        beta_prev = beta_curr;
        if (del.n_elem > 0) beta_prev.rows(del).zeros();
      }
    }
    beta_mat.col(i) = beta_prev;
  }
  return beta_mat;
}

arma::mat Prediction_Grid(const arma::mat & x_test,
                          const arma::mat & x_train,
                          const arma::vec & y_train,
                          const arma::mat & grid_betas) {
  arma::mat predictions = x_test * grid_betas;
  predictions += mean(y_train);
  predictions.each_row() -= mean(x_train) * grid_betas;
  return predictions;
}

arma::mat CrossValidation(const arma::mat & x, const arma::colvec & y,
                          double q, const arma::vec & lambda,
                          double converge, double eta,
                          arma::uword num_folds, arma::uword num_threads,
                          const arma::mat * R = nullptr,
                          const arma::mat * r = nullptr) {
  const arma::uword n = x.n_rows;
  // Keep the original integer linspace boundaries, including uneven folds.
  const arma::uvec boundaries = linspace<uvec>(0, n, num_folds + 1);
  arma::mat mses(lambda.n_elem, num_folds, fill::zeros);
  std::vector<std::string> errors(num_folds);
#ifdef _OPENMP
  const int workers = static_cast<int>(std::min(num_threads, num_folds));
#pragma omp parallel for num_threads(workers)
#endif
  for (int fold = 0; fold < static_cast<int>(num_folds); ++fold) {
    try {
      const arma::uvec train = TrainingRows(n, boundaries(fold), boundaries(fold + 1));
      const arma::mat x_train = x.rows(train);
      const arma::vec y_train = y.rows(train);
      const arma::mat x_test = x.rows(boundaries(fold), boundaries(fold + 1) - 1);
      const arma::vec y_test = y.rows(boundaries(fold), boundaries(fold + 1) - 1);
      const arma::mat betas = R == nullptr ?
        arma::mat(Bridge(x_train, y_train, q, lambda, converge, eta)) :
        RBridge(x_train, y_train, q, lambda, *R, *r, converge, eta);
      const arma::mat preds = Prediction_Grid(x_test, x_train, y_train, betas);
      // Preserve the historical error normalization and accumulation order.
      const arma::vec scaled_y = y_test / sqrt(n);
      for (arma::uword i = 0; i < lambda.n_elem; ++i) {
        mses(i, fold) = accu(square(scaled_y - preds.col(i) / sqrt(n)));
      }
    } catch (const std::exception & error) {
      // No R API calls inside worker threads. Report errors after the join.
      errors[fold] = error.what();
    } catch (...) {
      errors[fold] = "unknown numerical error";
    }
  }
  for (arma::uword fold = 0; fold < num_folds; ++fold) {
    if (!errors[fold].empty()) {
      throw std::runtime_error("CV fold " + std::to_string(fold + 1) +
        ": " + errors[fold]);
    }
  }
  return mses;
}

// [[Rcpp::export]]
arma::mat CV_Bridge(const arma::mat & x,
                    const arma::colvec & y,
                    const double & q,
                    arma::vec & lambda,
                    const double & converge,
                    const double & eta,
                    const arma::uword & num_folds,
                    const arma::uword & num_threads) {
  return CrossValidation(x, y, q, lambda, converge, eta, num_folds, num_threads);
}

// [[Rcpp::export]]
arma::mat CV_RBridge(const arma::mat & x,
                     const arma::colvec & y,
                     const double & q,
                     arma::vec & lambda,
                     const arma::mat & R,
                     const arma::mat & r,
                     const double & converge,
                     const double & eta,
                     const arma::uword & num_folds,
                     const arma::uword & num_threads) {
  return CrossValidation(x, y, q, lambda, converge, eta, num_folds, num_threads, &R, &r);
}
