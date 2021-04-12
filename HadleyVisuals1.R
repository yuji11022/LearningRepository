library(tidyverse)
mpg
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
colnames(mpg)
mpg
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = manufacturer, y = cty))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = year, y = cty))

ggplot(data = mpg) + geom_point(mapping = aes(x = class, y = drv))


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, stroke = class))
View(mpg)

# Left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Right
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy),color="blue", shape=18)
