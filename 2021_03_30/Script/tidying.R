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

# create master table for brand, product, description by selecting relevant
# columns and filtering for distinct/unique observations
# test this against other dataframes to check relationship to foreign keys

master <- allShades %>% 
  select(brand, product, description) %>% 
  distinct()

write_csv(master, 'master.csv')
