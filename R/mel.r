mel <- function(method=1,...) {
if (method==1)
  ans <- mel1(...)
  else if (method==2)
    ans <- mel2(...)
  else if (method==3)
    ans <- mel3(...)
    else ans <- mel4(...)
ans
}

