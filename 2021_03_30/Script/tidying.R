# how does each datatable relate to the others
## which is the master and what are the primary and foreign keys in each

# deleted previous code - no unique identifiing variables found in
## allNumbers, allShades, allCategories
### only if uses imgSrc as a variable - duplicate images for same
### products

sephora %>% 
  group_by(product, imgAlt) %>% 
  count() %>% 
  filter(n>1)


