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
  geom_point(mapping = aes(x = displ, y = hwy))
View(mpg)

# Left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# Right
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy),color="blue", shape=18)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ class)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(~ cty)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(drv~ class, nrow = 2)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype=drv, color=drv))+geom_point(mapping =aes(x = displ, y = hwy, color=drv ), size=2)

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv, color=drv))


ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class %in% c("subcompact","midsize")), se = FALSE, mapping=aes(color=class))
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color= class)) + 
  geom_smooth(data = filter(mpg, class %in%class ), se = FALSE, mapping=aes(color=class)) #not sure why I need class %in%class

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_boxplot(mapping = aes(color = class)) + 
  geom_smooth()

