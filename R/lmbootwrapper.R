lmbootwrapper <-
function(j,wr1,wr2,x.pred,y.pred,n,cbb){
  tryCatch(lmboot(j,wr1,wr2,x.pred,y.pred,n,cbb),error=function (e) {
    warningcount <<- warningcount + 1
    lmbootwrapper(j,wr1,wr2,x.pred,y.pred,n,cbb)
  })
      }
