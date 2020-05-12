qplot(age, color=y, data=bank)
qplot(age, job,  color=y, data=bank)

qplot(poutcome, data=bank, ylab="Number of Records", fill=poutcome)
qplot(poutcome, color=y, data=bank, xlab="poutcome", ylab="Number of answers")+facet_grid(~y)

qplot(y, data=bank)

qplot(pdays,  color=y, ylab="Number of Records", data=bank)


ggplot(df, aes(x = Day, y = Mean)) +
  geom_bar(stat = "identity", width = 1) +
  theme_bw(base_size = 12) 