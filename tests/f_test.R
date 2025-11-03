

f_critical = qf(1-alpha,df1,df2)
f_critical
p_value = 1 - pf(f_statistic,df1,df2)
p_value
# f_test can only be right sided
compareRightSided(f_statistic,f_critical)
comparePvalueAlpha(p_value,alpha)