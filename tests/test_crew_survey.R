#Testing Plot combinations for plot_crew_survey
varName.options <- c(
  'port',
  'fishery',
  'demographic',
  'personal_life',
  'history',
  'employment_history',
  'employment_conditions',
  'compensation',
  'expenses',
  'satisfaction',
  'perceptions'
)
report.options <- c('MidAtlantic', 'NewEngland')
plottype.options <- c('ts', 'bar')

plot.combs <- expand.grid(
  varName = varName.options,
  report = report.options,
  plottype = plottype.options
)

i <- 1
pdf(here::here('crew_survey_combinations.pdf'), width = 10, height = 6)
for (i in 1:nrow(plot.combs)) {
  # plot.title <- paste(
  #   plot.combs$report[i],
  #   plot.combs$varName[i],
  #   plot.combs$plottype[i],
  #   sep = ': '
  # )

  p <- ecodata::plot_crew_survey(
    report = plot.combs$report[i],
    varName = plot.combs$varName[i],
    plottype = plot.combs$plottype[i],
    n = 0
  )

  print(p)
}
dev.off()
