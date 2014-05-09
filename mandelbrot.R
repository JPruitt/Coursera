resolution <- 0.001

sequence <- seq(-1, 1, by = resolution)

m <- matrix(nrow = length(sequence), ncol = length(sequence))

for (x in sequence)
{
  for (y in sequence)
  {
    mandelbrot <- in.mandelbrot.set(complex(real = x, imaginary = y))
    m[round((x + resolution + 1) / resolution), round((y + resolution + 1) / resolution)] <- mandelbrot
  }
}

png('mandelbrot.png')
image(m)
dev.off()
