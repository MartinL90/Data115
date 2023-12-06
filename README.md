# Data115
##Sort the data, create a new dataset contains data year from 2006 to 2011.
vgsales.year <- filter(vgsales, Year %in% c(2006, 2007, 2008, 2009, 2010, 2011))

##Create six histograms to show global sales of different genres.
ggplot(vgsales.year, aes(x=Global_Sales,color=Genre))+geom_histogram()+ggtitle("Video Games Sales From 2006 to 2011")+xlim(c(0,15))+ylim(c(0,350))+facet_wrap(~Year)

##create a new column to sum NA EU JP Other sales.
vgsales.year$total_Sales <- rowSums(vgsales.year[, c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales")])

##Compare the total_sales with Global sales
vgsales.year %>% mutate_at(vars(total_Sales, Global_Sales), funs(round(., 2)))
vgsales.year$Correct<-with(vgsales.year,ifelse(total_Sales==Global_Sales,'Yes','No'))

##Seem like there are some round up difference between total_Sales and Global_Sales, so create an additional colunm by comparing absolute value of the difference if it is less than 0.2. 
vgsales.year$Correct2<-with(vgsales.year,ifelse(abs(total_Sales-Global_Sales)<0.2,'Yes','No'))

##Confirm that there is not difference more than 0.2.
sum(vgsales.year$Correct2 == 'No')

##Sumarry the dataframe
summary(vgsales.year)

##Create a summary for sum of Global Sales and group by Year and Genre
result_dplyr <- vgsales.year %>%
+     group_by(Year, Genre) %>%
+     summarise(Sum_Global_Sales = sum(Global_Sales))

##create a scatter plot
ibrary(ggplot2)
ggplot(vgsales.year, aes(x = Year, y = Global_Sales, color = Genre)) +
+     geom_point() +
+     labs(title = "Scatter Plot of Global Sales Over Years by Genre",
+          x = "Year",
+          y = "Global Sales",
+          color = "Genre")

##create six bar chart for different years.
subset_data <- vgsales.year[vgsales.year$Year %in% c(2006, 2007, 2008, 2009, 2010, 2011), ]
ggplot(subset_data, aes(x = Genre, y = Global_Sales, fill = Genre)) +
+     geom_bar(stat = "identity") +
+     facet_wrap(~Year, scales = "free_x") +
+     labs(title = "Global Sales by Genre for Each Year",
+          x = "Genre",
+          y = "Global Sales",
+          fill = "Genre") +
+     theme(axis.text.x = element_text(angle = 45, hjust = 1))

##Create better view of Bar Chart
# Subset the data for the specified years
subset_data <- vgsales.year[vgsales.year$Year %in% c(2006, 2007, 2008, 2009, 2010, 2011), ]
# Calculate the sum total of Global Sales for each combination of Year and Genre
total_sales <- subset_data %>%
  group_by(Year, Genre) %>%
  summarise(Sum_Global_Sales = sum(Global_Sales))
# Create a bar chart for each year with total sales labels and set y-axis limit to 140
ggplot(subset_data, aes(x = Genre, y = Global_Sales, fill = Genre)) +
  geom_bar(stat = "identity") +
  geom_text(data = total_sales, aes(x = Genre, y = Sum_Global_Sales, label = scales::comma(Sum_Global_Sales)),
            vjust = -0.5, size = 3) +  # Adjust vjust as needed
  facet_grid(Year ~ ., scales = "free_x", space = "free_x", switch = "y") +
  labs(title = "Global Sales by Genre for Each Year",
       x = "Genre",
       y = "Global Sales",
       fill = "Genre") +
  ylim(0, 160) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
