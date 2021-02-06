# Tasks

fin.df=read.csv("MATH4753-Applied Statistical Methods/LABS/Lab 3/FINTUBES.csv")
spruce.df=read.csv("MATH4753-Applied Statistical Methods/LABS/Lab 3/SPRUCE.csv")#MS pg478
## Task 1
getwd()

## Task 2
head(fin.df)

## Task 3

  #a.
plot(spruce.df$BHDiameter, spruce.df$Height, main = "Spruce Height vs. BHDiameter",
     xlab = "BHDiameter(cm)", ylab = "Height(m)", pch = 21, bg = "Blue", cex = 1.2,
     ylim = c(0, 1.1 * max(spruce.df$Height)), xlim = c(0, 1.1 * max(spruce.df$BHDiameter)))

  #b. straight line relationship? Yes

  #c.

library(s20x)
layout(matrix(1:4, nrow = 2, ncol = 2, byrow = T))
layout.show(4)
trendscatter(Height~BHDiameter, f = .5 ,data = spruce.df)
trendscatter(Height~BHDiameter, f = .6 ,data = spruce.df)
trendscatter(Height~BHDiameter, f = .7 ,data = spruce.df)

  #d.
spruce.lm = with(spruce.df, lm(Height~BHDiameter))

  #e.
abline(spruce.lm)

  #f.
