library(networkD3)
library(magrittr)

data.sources <- data.frame(type='Data Source', name=c('social media', 'engagement metrics', 
                                   'comments',
                                  'social networks','conversions', 
                                  'message text', 'videos'), id=12:18)
old.data.sources <- data.frame(type='Conventional Data Source', name=c('landscape survey', 'focus groups', 
                                                                       'message tests','tracking survey'), id=8:11)
statistical.methods <- data.frame(type='Data Mining', name=c('consumer segmentation', 'factor analysis',
                                         'structural equations','causal models',
                                  'media mix optimization','adaptive delivery','deep language models', 'image parsers'), id=19:26)
old.statistical.methods <- data.frame(type='Conventional Data Mining', name=c('crosstabs', 'maxdiff',
                                                                              'conjoint analysis','response models',
                                             'significance tests' ), id=3:7)
phases <- data.frame(type='Phase', name=c('listen', 'create', 'deploy'), id=0:2)



all.nodes <- rbind(data.sources,old.data.sources, statistical.methods, old.statistical.methods, phases)
all.nodes$name <- factor(all.nodes$name, levels=all.nodes$name)

links <- data.frame(start=c('landscape survey', 
                            'focus groups',
                            'message tests',
                            'tracking survey',
                            'social media',
                            'engagement metrics',
                            'comments',
                            'social networks',
                            'conversions', 
                            'message text', 'videos',
                            'listen', 
                            'create',
                            'consumer segmentation', 'factor analysis',
                            'structural equations','causal models',
                            'media mix optimization','adaptive delivery',
                            'deep language models', 'image parsers',
                            'crosstabs', 'maxdiff',
                            'conjoint analysis','response models',
                            'significance tests','significance tests'), 
                    end=c('listen', 
                          'listen',
                          'create',
                          'deploy', 
                          'listen', 
                          'deploy',
                          'deploy',
                          'deploy',
                          'deploy',
                          'deploy',
                          'create',
                          'create', 
                          'deploy', 'listen','listen','listen','listen',
                          'deploy', 'listen','deploy','listen', 'listen', 
                          'listen', 'listen','deploy','deploy','create'))

links$chart1 <- 0
links$chart1[1:4] <- 1
links$chart1[12:13] <- 1
links$chart1[22:27] <- 1

all.nodes$chart1 <- all.nodes$name %in% c(links$start[links$chart1==1] %>% as.character , links$end[links$chart1==1]  %>% as.character)
  
links$start1 <- (factor(links$start, levels=levels(factor(all.nodes$name[all.nodes$chart1]))) %>% as.numeric )-1
links$end1 <- (factor(links$end, levels=levels(factor(all.nodes$name[all.nodes$chart1]))) %>% as.numeric )-1


forceNetwork(Links = links[links$chart1==1,],charge = -500, Nodes = all.nodes[all.nodes$chart1,],
             Source = "start1", Target = "end1",NodeID = "name",
             Group = "type", opacity = 0.8,height = 800,width = 800,
             colourScale = JS("d3.scaleOrdinal(['blue','orange','darkgreen']);"), fontSize=20,opacityNoHover=1)


links$start2 <- (factor(links$start, levels=levels(factor(all.nodes$name))) %>% as.numeric )-1
links$end2 <- (factor(links$end, levels=levels(factor(all.nodes$name))) %>% as.numeric )-1


forceNetwork(Links = links, Nodes = all.nodes,charge = -500,
             Source = "start2", Target = "end2",NodeID = "name",
             Group = "type", opacity = 0.8,height = 800,width = 800,
             colourScale = JS("d3.scaleOrdinal(['blue','#0000b2','#cc8400','orange','darkgreen']);"), fontSize=20,opacityNoHover=1)

######An alternative


phases.two <- data.frame(type='Phase2', name=c('listen','interpret','create' ,'respond'), id=19:20)

all.nodes <- rbind(data.sources,old.data.sources, statistical.methods, old.statistical.methods, phases.two)
all.nodes$name <- factor(all.nodes$name, levels=all.nodes$name)



links <- data.frame(start=c('listen','interpret','create', 'respond'), 
                    end=c('interpret','create' ,'respond', 'listen'))
links.to.ds <- data.frame(start=c('landscape survey', 'focus groups', 
                                  'message tests','tracking survey',
                                  'social media', 'engagement metrics', 
                                  'comments',
                                  'social networks','conversions', 
                                  'message text', 'videos'), 
                          end=c('listen','listen', 'create', 'listen', 'listen',
                                'respond', 'listen', 'create', 'listen', 
                                'interpret', 'interpret'
                                ))
links.to.an <- data.frame(start=c('crosstabs', 'maxdiff',
                                  'conjoint analysis','response models',
                                  'significance tests',
                                  'consumer segmentation', 'factor analysis',
                                  'structural equations','causal models',
                                  'media mix optimization','adaptive delivery',
                                  'deep language models', 'image parsers'), 
                          end=c('interpret','interpret','interpret', 'respond', 'respond', 
                                'listen', 'listen', 'interpret', 'interpret', 'respond', 'respond', 'create','create'))
links <- rbind(links, links.to.an, links.to.ds)

links$start2 <- (factor(links$start, levels=levels(factor(all.nodes$name))) %>% as.numeric )-1
links$end2 <- (factor(links$end, levels=levels(factor(all.nodes$name))) %>% as.numeric )-1


forceNetwork(Links = links, Nodes = all.nodes,charge = -500,
             Source = "start2", Target = "end2",NodeID = "name",
             Group = "type", opacity = 0.8,height = 1000,width = 1200,
             colourScale = JS("d3.scaleOrdinal(['blue','#0000b2',
                              '#cc8400','orange','darkgreen']);"), fontSize=20,opacityNoHover=1)


