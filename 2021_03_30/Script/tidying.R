# how does each datatable relate to the others
## which is the master and what are the primary and foreign keys in each

### allShades

allShades %>% 
  group_by(brand, product, description, imgSrc) %>% 
  count() %>% 
  filter(n == 1) %>% 
  view()

### this group of variables = primary key. there a duplicates
### for (brand, product, description) due to multiple image urls for 
### same product 

