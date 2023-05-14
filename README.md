# Gardening Google Trends
*Small project to practice using R and Datawrapper.*

## Write-up and Datawrapper visualisations

With the weather warming up in Newcastle upon Tyne, I’ve been spending more time in our new garden. As a novice gardener, I’ve also been doing more research online, Googling how and when to plant my various vegetable and flower seeds. These searches made me wonder how other gardeners use Google. Do their searches also vary over the course of the year?

Using Google Trends data, I began investigating garden searches in the UK, focusing on “how to garden” to capture interest in hobby gardening. I soon found that the expected seasonal changes weren’t the most interesting pattern in recent years: 

<a href = "https://www.datawrapper.de/_/V90Wd/"><img src="plots/how_to_garden_dw_vis.png" width="700" alt="A line graph showing how interest in gardening, as captured by Google searches for how to garden, changed each month in the UK. Each year, gardening interest is higher in spring and summer than in autumn and winter. In 2020, after the March 2020 COVID lockdown, the seasonal interest is about double that of the previous years.">

*Click on the chart for an interactive version*

Compared to the previous five years, **2020 saw more than double the number of "how to garden" searches** after the first UK COVID-19 lockdown. In hindsight, the increased searches during 2020 are unsurprising, as **[the COVID-19 pandemic saw a dramatic increase in hobbiest gardeners](https://www.mind.org.uk/news-campaigns/news/over-7-million-have-taken-up-gardening-since-the-pandemic-new-research-shows-spending-more-time-in-nature-has-boosted-nation-s-wellbeing/).** Like the pandemic, this higher interest persisted into 2021, with 2022 also seeing somewhat elevated searches. Interestingly, 2021 also bucks the usual seasonal pattern: “how to garden” searches increased a month earlier than expected, suggesting that gardeners were eager to get back outdoors after a winter of lockdowns!

I can think of two main reasons for the increased gardening interest:
* **People looking for new at-home hobbies** for themselves or their families. Many people had more free time due to furlough or working from home, and many usual hobbies and social activities were restricted by COVID lockdowns.
* **People who wanted access to fresh food** without risking exposure to COVID-19 in shops or dealing with [strange grocery substitutions](https://www.theguardian.com/business/2023/mar/25/strawberries-instead-of-bin-liners-poll-reveals-strange-supermarket-delivery-substitutions). 
  
To better understand what drove the gardening Google searches, I investigated two more specific search terms: "how to grow vegetables" and "how to grow flowers."
  
 

## Stand-alone R visualisation

<img src="plots/R_gardening_COVID.png" width = 900> 

*Compared to the above plots, I've added a third, more generic search: "how to grow food." Food searches also increased during COVID, suggesting at least some gardening interest was driven by people who wanted to grow their own food.*
