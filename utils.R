# This file contains some useful functions

maxCol_row = inline::cfunction(sig = c(x = "matrix"), body = '
    int nr = INTEGER(getAttrib(x, R_DimSymbol))[0], nc = INTEGER(getAttrib(x, R_DimSymbol))[1]; 
    double *px = REAL(x), *buf = (double *) R_alloc(nr, sizeof(double));
    for(int i = 0; i < nr; i++) buf[i] = R_NegInf;

    SEXP ans = PROTECT(allocVector(INTSXP, nr));
    int *pans = INTEGER(ans);

    for(int i = 0; i < nr; i++) {
        for(int j = 0; j < nc; j++) {
            if(px[i + j*nr] > buf[i]) {
                buf[i] = px[i + j*nr];
                pans[i] = j + 1;
            }
        }
    }

    UNPROTECT(1);
    return(ans);
', language = "C")



binLogLoss <- function(target, pred){
  -sum(target*log(pred) + (1-target)*log(1-pred),na.rm=TRUE)
}


