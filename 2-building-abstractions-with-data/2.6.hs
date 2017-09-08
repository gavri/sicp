zero = (\_ -> id)
addOne n = (\f -> f . (n f))
add m n = (\f -> (m f) . (n f))
