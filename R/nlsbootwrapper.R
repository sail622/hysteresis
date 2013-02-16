nlsbootwrapper <-
function(j,wr1,wr2,x.pred,y.pred,n,cbb){
          tryCatch(nlsboot(j,wr1,wr2,x.pred,y.pred,n,cbb),error=function (e) {
           warningcount <<- warningcount + 1
            nlsbootwrapper(j=NULL,wr1,wr2,x.pred,y.pred,n,cbb)
           })
      }
