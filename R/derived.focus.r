derived.focus <- function (lambda1,lambda2,theta) {
focus.distance <- sqrt(lambda1^2-lambda2^2)
focus.x <- cos(theta)*focus.distance
focus.y <- sin(theta)*focus.distance
focus.constant <- lambda1*2
z <- c("focus.distance"=focus.distance,"focus.x"=focus.x,"focus.y"=focus.y,"focus.constant"=focus.constant)
z
}