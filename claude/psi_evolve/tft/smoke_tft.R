# Shape smoke test: build the graph, check every tensor, count params.
source("tft_model.R")
L <- 52L; H <- 12L; NS <- 12L; NP <- 39L; NF <- 38L; NC <- 40L
cat(sprintf("building TFT: lookback %d, horizon %d, static %d, past %d, future %d\n", L,H,NS,NP,NF))
m <- build_tft(n_static=NS, n_past=NP, n_future=NF, lookback=L, horizon=H,
               n_countries=NC, units=32L, n_heads=4L)
cat("output shape:", paste(unlist(m$output_shape), collapse=" x "), "\n")
cat("params:", format(m$count_params(), big.mark=","), "\n")
B <- 8L
x <- list(static  = array(runif(B*NS),      c(B,NS)),
          country = array(sample(0:(NC-1),B,TRUE), c(B,1L)),
          past    = array(runif(B*L*NP),    c(B,L,NP)),
          future  = array(runif(B*H*NF),    c(B,H,NF)))
y <- m(x)
cat("forward pass OK, out dims:", paste(dim(as.array(y)), collapse=" x "),
    "(expect", B, "x", H, "x 5 )\n")
lf <- make_quantile_loss(c(0.025,0.25,0.5,0.75,0.975))
yt <- array(runif(B*H*1), c(B,H,1L))
cat("quantile loss on random data:", as.numeric(lf(
      keras3::op_convert_to_tensor(yt, dtype="float32"), y)), "\n")
m$compile(optimizer = keras3::optimizer_adam(1e-3), loss = lf)
h <- m$fit(x, yt, epochs = 2L, verbose = 0L)
cat("2-epoch fit OK; loss:", paste(round(unlist(h$history$loss),5), collapse=" -> "), "\n")
cat("SMOKE PASS\n")
