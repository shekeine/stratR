#Read sample data
dat <- fread('data/data_wide.csv')

#Define variable types
precipvars <- c('JJAPdelta', 'DJFPdelta')
tempvars  <- c('DJFTdelta', 'JJATdelta')
modvars   <- c('minD')
metavars  <- c('ids', 'entitynum', 'region', 'site', 'londd', 'latdd', 'altitude', 'samplenum', 'agebp')
pollvars  <- names(dat)[!names(dat)%in%c(precipvars, tempvars, modvars, metavars)]

#Melt to long
dat_melt <- melt.data.table(data=dat, id.vars=metavars, variable.factor=F)

#Define variable groups
dat_melt[variable%in%precipvars, group:='Precipitation']
dat_melt[variable%in%tempvars, group:='Temperature']
dat_melt[variable%in%modvars, group:='Model']
dat_melt[variable%in%pollvars, group:='Pollen taxa']

#Run on sample data
test <- stratPlot(dat=dat_melt,
                   tymcol='agebp', varcol='variable', valcol='value',
                   varord=c('med'),
                   grp_ord=c('Model', 'Precipitation',
                             'Temperature', 'Pollen taxa'), grpcol='group',

                   grpgeom=c('line', 'line', 'line','area'),
                   grp_vartype=c('MinD', 'SD', 'SD','Percentages'),

                   const_grps=c('Model', 'Precipitation', 'Temperature'),
                   gcols=c('grey40', 'blue', 'brown3','darkgreen'),

                   #xlim1=c(0,0,0),
                   xbline=c(0, 0, 0, 0),
                   xbrkint=c(10, 20, 5, 10),
                   ybrkint=1000,

                   axs_tsize=12, var_tsize=15, grp_tsize=20, ptt_tsize=25,

                   plab='Entity 33', ylab='Age BP',
                   pmargin=unit(c(0.05, 0, 0.1, 0), 'cm'))

grid.newpage()
grid.draw(test)
