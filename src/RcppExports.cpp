// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// augment_loop
arma::mat augment_loop(arma::mat x, int n, double buff);
RcppExport SEXP _SensitivityFunctions2_augment_loop(SEXP xSEXP, SEXP nSEXP, SEXP buffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type buff(buffSEXP);
    rcpp_result_gen = Rcpp::wrap(augment_loop(x, n, buff));
    return rcpp_result_gen;
END_RCPP
}
// augment_loop2
arma::mat augment_loop2(arma::mat x, int n, double buff);
RcppExport SEXP _SensitivityFunctions2_augment_loop2(SEXP xSEXP, SEXP nSEXP, SEXP buffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type buff(buffSEXP);
    rcpp_result_gen = Rcpp::wrap(augment_loop2(x, n, buff));
    return rcpp_result_gen;
END_RCPP
}
// matprod
arma::mat matprod(arma::mat x);
RcppExport SEXP _SensitivityFunctions2_matprod(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(matprod(x));
    return rcpp_result_gen;
END_RCPP
}
// runifcpp
double runifcpp(double lower, double upper);
RcppExport SEXP _SensitivityFunctions2_runifcpp(SEXP lowerSEXP, SEXP upperSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type lower(lowerSEXP);
    Rcpp::traits::input_parameter< double >::type upper(upperSEXP);
    rcpp_result_gen = Rcpp::wrap(runifcpp(lower, upper));
    return rcpp_result_gen;
END_RCPP
}
// subset
arma::mat subset(arma::mat x, int row, int column_min, int column_max);
RcppExport SEXP _SensitivityFunctions2_subset(SEXP xSEXP, SEXP rowSEXP, SEXP column_minSEXP, SEXP column_maxSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type row(rowSEXP);
    Rcpp::traits::input_parameter< int >::type column_min(column_minSEXP);
    Rcpp::traits::input_parameter< int >::type column_max(column_maxSEXP);
    rcpp_result_gen = Rcpp::wrap(subset(x, row, column_min, column_max));
    return rcpp_result_gen;
END_RCPP
}
// vecmult
arma::mat vecmult(arma::mat x, arma::mat y);
RcppExport SEXP _SensitivityFunctions2_vecmult(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(vecmult(x, y));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_SensitivityFunctions2_augment_loop", (DL_FUNC) &_SensitivityFunctions2_augment_loop, 3},
    {"_SensitivityFunctions2_augment_loop2", (DL_FUNC) &_SensitivityFunctions2_augment_loop2, 3},
    {"_SensitivityFunctions2_matprod", (DL_FUNC) &_SensitivityFunctions2_matprod, 1},
    {"_SensitivityFunctions2_runifcpp", (DL_FUNC) &_SensitivityFunctions2_runifcpp, 2},
    {"_SensitivityFunctions2_subset", (DL_FUNC) &_SensitivityFunctions2_subset, 4},
    {"_SensitivityFunctions2_vecmult", (DL_FUNC) &_SensitivityFunctions2_vecmult, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_SensitivityFunctions2(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
