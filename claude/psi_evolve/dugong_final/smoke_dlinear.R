# Shape smoke test for the DLinear trunk: does it build and produce (B, units_3)?
suppressMessages(library(keras3))
`%||%` <- function(a,b) if (is.null(a)) b else a
hp <- list(units_3 = 32L, dropout = 0.1, dlinear_kernel = 5L)
T <- 13L; F <- 38L
input_feat <- keras3::layer_input(shape = c(T, F), name = "feat")
k <- as.integer(hp$dlinear_kernel); if (k %% 2L == 0L) k <- k + 1L
trend <- keras3::layer_average_pooling_1d(
     keras3::layer_zero_padding_1d(input_feat,
          padding = c((k-1L)%/%2L, (k-1L)%/%2L), name="dlin_pad"),
     pool_size = k, strides = 1L, name = "dlin_trend")
cat("trend shape:", paste(unlist(trend$shape), collapse=" x "), "(expect NULL x",T,"x",F,")\n")
remainder <- keras3::op_subtract(input_feat, trend)
lin <- function(z, nm) {
     z <- keras3::layer_permute(z, dims = c(2L,1L), name=paste0(nm,"_perm"))
     z <- keras3::layer_dense(z, units = as.integer(hp$units_3), name=paste0(nm,"_lin"))
     keras3::layer_flatten(z, name=paste0(nm,"_flat"))
}
z <- keras3::op_add(lin(trend,"dlin_t"), lin(remainder,"dlin_r"))
z <- keras3::layer_dense(z, units = as.integer(hp$units_3), name="dlin_proj")
z <- keras3::layer_dropout(z, rate = hp$dropout, name="dlin_do")
m <- keras3::keras_model(inputs = input_feat, outputs = z, name="dlinear_trunk")
cat("trunk output:", paste(unlist(m$output_shape), collapse=" x "), "(expect NULL x 32)\n")
cat("params:", format(m$count_params(), big.mark=","), "\n")
y <- m(array(runif(4L*T*F), c(4L,T,F)))
cat("forward OK:", paste(dim(as.array(y)), collapse=" x "), "\n")
cat("SMOKE PASS\n")
