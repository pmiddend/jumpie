x0 :: Double
x0 = 380.0

y0 :: Double
y0 = 370.0

vxmax = 6.0

grv = 0.21875

vymax = -6.5

p :: Double -> (Double,Double)
p t = (x0 + vxmax * t, grv/2.0 * t * t + vymax *t + y0) 

yl = 310.0

intersection :: Double
intersection = (-1) * sqrt ( (2.0*(yl-y0))/grv + (vymax*vymax)/(grv*grv) ) - vymax/grv
