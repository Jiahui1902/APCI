
# hexagram ####
#matrix: age as rows, period as columns first_age = 0,
apci.plot.hexagram <- function(model, #matrix: age as rows, period as columns first_age,
                           age,
                           period,
                           first_age,
                           first_period,
                           interval,
                           first_age_isoline = NULL,
                           first_period_isoline = NULL,
                           isoline_interval = NULL,
                           color_scale = NULL,
                           color_map = NULL,
                           line_width = .5,
                           line_color = "grey",
                           label_size = .5,
                           label_color = "black",
                           scale_units = "Quintile",
                           wrap_cohort_labels = TRUE,
                           quantile = NULL){
  data <- model$int_matrix
  data.raw <- as.data.frame(model$model$model)
  data.raw[,age] <- data.raw$acc
  data.raw[,period] <- data.raw$pcc

  data$period <- rep(1:nlevels(data.raw[,period]),
                     each = nlevels(data.raw[,age]))%>%as.factor
  data$age <-
    rep(1:nlevels(data.raw[,age]),
        nlevels(data.raw[,period]))%>%as.factor
  data$value <- data$iaesti%>%as.character%>%as.numeric
  data <- data.table::dcast(data.table::as.data.table(data),
                            age~period,value.var = "value")%>%
    as.data.frame%>%.[,-1]%>%as.matrix
  nnrow <- nrow(data)
  nncol <- ncol(data)

  if(!is.null(quantile)){
  data <- cut(data,quantile(data,probs = seq(0,1,quantile)),
                include.lowest = T,
                labels = quantile(data,
                                  probs = seq(0,1,quantile))[-1])
  }
  data <- as.numeric(data)
  data <- matrix(data,nrow=nnrow,ncol=nncol)

  # setting default values for missing parameters
  if(is.null(first_age_isoline)){
    first_age_isoline = first_age
    }
  if(is.null(first_period_isoline)){
    first_period_isoline = first_period
    }
  if(is.null(isoline_interval)){
    isoline_interval = 2 * interval }
  if(is.null(color_scale)){ #if color scale is missing use the min and max of data
    color_scale[1] <- min(data)
    color_scale[2] <- max(data)
    }
  if(is.null(color_map)){
    # define jet colormap
    jet.colors <- colorRampPalette(c("black", "#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red",
                                     "#7F0000"))
    color_map = jet.colors(100)
  }else{
    jet.colors <- colorRampPalette(c(color_map[1],color_map[2]))(100)
    color_map = jet.colors
  }

  # end of default values

  m <- dim(data)[1]
  n <- dim(data)[2]

  last_age = first_age + (m - 1) * interval
  last_period = first_period + (n - 1) * interval
  first_cohort = first_period - last_age
  last_cohort = last_period - first_age

  age_isolines = seq(from = first_age_isoline, to = last_age, by = isoline_interval)
  period_isolines = seq(from = first_period_isoline, to = last_period, by = isoline_interval)
  last_age_isoline = tail(age_isolines,1)
  first_cohort_isoline = first_period_isoline - last_age_isoline
  cohort_isolines = seq(from = first_cohort_isoline, to = last_cohort, by = isoline_interval)

  periods <- seq(from = first_period, to = last_period, by = interval)
  ages <- seq(from = first_age, to = last_age, by = interval)
  cohorts <- seq(from = first_cohort, to = last_cohort, by = interval)
  n_ages <- length(ages)
  n_periods <-length(periods)
  n_cohorts <- length(cohorts)

  n_age_isolines <- length(age_isolines)
  n_period_isolines <- length(period_isolines)
  n_cohort_isolines <- length(cohort_isolines)

  # apply the limits to the data by truncating it
  data[data<color_scale[1]] = color_scale[1]
  data[data>color_scale[2]] = color_scale[2]

  # === plotting ====
  ncol <- length(color_map)
  not_nan_data <- !is.nan(data)
  v_data <- as.vector(data[not_nan_data])
  datac = cut(data[not_nan_data], #discretize the data
              seq(from = color_scale[1], to = color_scale[2], length.out = ncol), include.lowest = T,
              labels = F)
  a <- interval / sqrt(3) # radius of the hexagon (distance from center to a vertex).
  b <- sqrt(3)/2 * a # half height of the hexagon (distance from the center perpendicular to the middle of the top edge)
  yv <- c(0, b, b, 0, -b, -b, 0)
  xv <- c(-a, -a/2, a/2, a, a/2, -a/2, -a)
  # compute the center of each hexagon by creating an a*p grid for each age-period combination
  P0 <- matrix(periods, nrow = n_ages, ncol=n_periods, byrow = TRUE)
  A0 <- t(matrix(ages, nrow = n_periods, ncol = n_ages, byrow = TRUE))
  # convert the grid to the X-Y coordinate
  X <- compute_xcoordinate(P0)
  Y <- compute_ycoordinate(P0, A0)
  # only keep those that have non-NA values
  X <- X[not_nan_data]
  Y <- Y[not_nan_data]
  # get the color for each level
  color_map2 <- color_map[datac]

  Xvec <- as.vector(X)
  Yvec <- as.vector(Y)
  n_hexagons <- length(Xvec)

  # compute the X and Y cooridinate for each hexagon - each hexagon is a row and each point is a column
  Xhex <- outer(Xvec, xv, '+')
  Yhex <- outer(Yvec, yv, '+')

  minX <- min(Xhex) - interval
  maxX <- max(Xhex) + interval

  if (wrap_cohort_labels){
    minY <- min(Yhex) - interval
  } else {
    minY <- compute_ycoordinate(p=first_period, a=first_age - (last_period-first_period)) - interval
  }
  maxY <- max(Yhex) + interval
  layout(t(1:2),widths=c(4,1)) # two columns - one for the plot, the other for the colorbar

  par(mar=c(.5,.5,.5,.5))

  plot(x = NULL, y = NULL,
       xlim = c(minX,maxX),
       ylim = c(minY,maxY),
       axes=FALSE, frame.plot=FALSE, xaxt = 'n', yaxt = 'n', type = 'n', asp = 1)

  for (i in 1:n_hexagons){
    polygon(x = Xhex[i,],
            y = Yhex[i,],
            col = color_map2[i],
            border = NA, # Color of polygon border lwd = 1)
            lwd = 1)
  }

       #age-isolines
       y1 <- compute_ycoordinate(first_period,age_isolines)
       y2 <- compute_ycoordinate(last_period+ interval,age_isolines)
       x1 <- compute_xcoordinate(first_period)
       x2 <- compute_xcoordinate(last_period + interval)

       for (i in 1:n_age_isolines){
         lines(x=c(x1,x2), y=c(y1[i],y2[i]), col = line_color, lwd = line_width)
         text(x=x2, y=y2[i], labels = paste("A:",age_isolines[i]),
          col = label_color, cex = label_size, srt = -30,
          adj = c(0, 0.5))
       }

       # period-isolines
       x <- compute_xcoordinate(period_isolines)
       y1 <- compute_ycoordinate(period_isolines, first_age)
       y2 <- compute_ycoordinate(period_isolines, last_age+interval)
       for (i in 1:n_period_isolines){
         lines(x=c(x[i], x[i]), y=c(y1[i],y2[i]), col = line_color, lwd = line_width)
         text(x=x[i], y=y2[i], labels = paste("P:",period_isolines[i]),
       col = label_color, cex = label_size, srt = 90, adj = c(0, .5)) #pos = 4)
       }

         # cohort-isolines (need some more processing!)
         # determine the periods where the cohort isolines cross the last age
         p_top <- cohort_isolines + last_age
         p_top <- p_top[p_top < last_period]
         n_top <- length(p_top)
         # and the periods where they cross the first age
         p_bottom <- cohort_isolines + first_age
         p_bottom <- p_bottom[p_bottom > first_period]
         n_bottom <- length(p_bottom)
         # and the ages where they cross the first period
         a_left <- first_period - cohort_isolines
         if (wrap_cohort_labels){
           a_left <- a_left[a_left >= first_age]
           }
         n_left <- length(a_left)
         # and the ages where they cross the last period
         a_right <- last_period - cohort_isolines
         a_right <- a_right[a_right <= last_age]
         n_right <- length(a_right)
         # combine the periods and ages initial and final points on the a*p coordinates
         # first the left-bottom edge
         if (wrap_cohort_labels){
         p1 <- c(rep(first_period, n_left), p_bottom)
         a1 <- c(a_left, rep(first_age, n_bottom))
} else {
  p1 <- c(rep(first_period, n_left))
  a1 <- c(a_left)
  }
# then the top-right edge
p2 <- c(p_top, rep(last_period, n_right))
a2 <- c(rep(last_age, n_top), a_right)

# convert the a*p coordinates to x-y coordinates
x1 <- compute_xcoordinate(p1-interval) #,a1-1)
x2 <- compute_xcoordinate(p2) #,a2)
y1 <- compute_ycoordinate(p1-interval, a1-interval)
y2 <- compute_ycoordinate(p2, a2)
# finally draw the lines.
for (i in 1:n_cohort_isolines){
  lines(x=c(x1[i], x2[i]),
        y=c(y1[i],y2[i]),
        col = line_color, lwd = line_width)
  text(x=x1[i], y=y1[i], labels = paste("C:",cohort_isolines[i]+n_age_isolines),
       col = label_color, cex = label_size, srt = 30,
       adj = c(1,0.5))
}

# create the colorbar
par(las=2)
par(mar=c(10,2,10,2.5))
cb_range <- seq(from = color_scale[1], to = color_scale[2], length.out = ncol)
image(y=cb_range,z=t(cb_range), col=color_map, axes=FALSE, main=scale_units, cex.main=.8)
axis(4,cex.axis=label_size,mgp=c(0,.5,0))
}

compute_xcoordinate <- function(p) { x <- p * sqrt(3) / 2
return(x)
}

compute_ycoordinate <- function(p, a){ y <- a - p / 2
return(y) }



# heatmap ####
apci.plot.heatmap <- function(model,
                              age,
                              period,
                              color_map = NULL,
                              color_scale = NULL,
                              quantile = NULL,
                              ...){
  data <- model$int_matrix
  data.raw <- as.data.frame(model$model$model)
  data.raw[,age] <- data.raw$acc
  data.raw[,period] <- data.raw$pcc

  data$period <- rep(1:nlevels(data.raw[,period]),
                     each = nlevels(data.raw[,age]))%>%as.factor
  data$age <-
    rep(1:nlevels(data.raw[,age]),
        nlevels(data.raw[,period]))%>%as.factor

  data$value <- data$iaesti%>%as.character%>%as.numeric

if(!is.null(quantile)){
  data$value <- cut(data$value,quantile(data$value,
                                        probs = seq(0,1,quantile)),
                    include.lowest = T,
                    labels = quantile(data$value,
                                      probs = seq(0,1,quantile))[-1])
  data$value <- as.numeric(data$value)
  color_scale <- c(min(data$value,na.rm = T),
                   max(data$value,na.rm = T))
  color_scale[1] <- round(color_scale[1],2)#-0.01
  color_scale[2] <- round(color_scale[2],2)#+0.01
  bk <- seq(1,1/quantile,1)
  # nm <- "Age-Period Interaction\nQuantile"
  nm <- "Deviation (Quantile)"
}else{
  color_scale <- c(min(data$value,na.rm = T),
                   max(data$value,na.rm = T))
  color_scale[1] <- round(color_scale[1],2)-0.01
  color_scale[2] <- round(color_scale[2],2)+0.01
  bk <- seq(color_scale[1],color_scale[2],
            (color_scale[2]-color_scale[1])/5)
  # nm <- "Age-Period Interaction"
  nm <- "Deviation"
}

  if(is.null(color_map)){
    color_map <- colorRampPalette(c('white','black'))(100)
  }else{
    color_map <- colorRampPalette(c(color_map[1],color_map[2]))(100)
  }

  g <- ggplot2::ggplot(data,
                       ggplot2::aes(x=period,y=age,fill=value))+
    ggplot2::geom_tile()+
    # geom_text(label = data$iasig,color = "green",size = 5)+ # remove the stars
    ggplot2::coord_equal()+
    ggplot2::theme_bw()+
    ggplot2::scale_fill_gradientn(colors = color_map,
                       # low = color_map[1],
                       # high = color_map[length(color_map)],
                       name=nm,
                       breaks = bk,
                       limits = color_scale)+
    ggplot2::theme(legend.title = ggplot2::element_text(size=8))
  ggplot2::labs(x = 'Period Group',
       y = 'Age Group')
model$cohort_average$cohort_index <- seq(nlevels(data.raw[,age])-1,
                                         -nlevels(data.raw[,period])+1,-1)

# run <- lapply(model$cohort_average$cohort_index[model$cohort_average$sig!="   "],
run <- lapply((1:nrow(model$cohort_average))[model$cohort_average$sig!="   "],
       function(i){
         if(is.na(model$cohort_slope[i,"sig"])){
 intercept <- model$cohort_average$cohort_index[i]
 g<<-g+
   ggplot2::geom_abline(intercept = intercept,
                        slope = 1,color = "green",linetype='dotted')
         }else{
         if(model$cohort_slope[i,"sig"]=="   "){
         intercept <- model$cohort_average$cohort_index[i]
  g<<-g+
    ggplot2::geom_abline(intercept = intercept,
                         slope = 1,color = "green",linetype='dotted')
         }else{
           if((model$cohort_average[i,"cohort_average"]%>%as.character%>%as.numeric)>0){
         intercept <- model$cohort_average$cohort_index[i]
  g<<-g+
    ggplot2::geom_abline(intercept = intercept,
              slope = 1,color = "green",linetype='solid')
           }else{
         intercept <- model$cohort_average$cohort_index[i]
  g<<-g+
    ggplot2::geom_abline(intercept = intercept,
                           slope = 1,color = "green",linetype='dashed')
           }
         }
         }
  })
g+
  ggplot2::labs(caption = "Line:\n average cohort effect is significantly different from the main effect
       Intra-cohort change:\n solid positive; dashed: negative; dotted: no change")
}

# model <- APC_I
# age <- "acc"
# period <- "pcc"
#
# apci.plot.heatmap(model = APC_I,
#                   age = "acc",period = 'pcc')

# line.raw ####
apci.plot.raw <- function(data,
                             outcome_var,
                             age,
                             period,
                             ...){
  data <- as.data.frame(data)
  data$outcome_var <- data[,outcome_var]
  data$age <- data[,age]%>%as.factor
  data$period <- data[,period]%>%as.factor

  g1 <- ggplot2::ggplot(data,
               ggplot2::aes(x=period,group=age,
                            y = outcome_var,col=age))+
    ggplot2::geom_point()+
    ggplot2::geom_path()+
    ggplot2::theme_bw()+
    ggplot2::labs(x = "Period Group",names = "Age",
                  y = as.character(outcome_var))+
    ggplot2::geom_point(data = data %>%
                dplyr::group_by(period) %>%
                dplyr::summarise(outcome_var = mean(outcome_var,
                                                    na.rm = T)),
              mapping = ggplot2::aes(x = period,
                                     group=NA,y=outcome_var,col=NA),
              size = 3,shape=8,color="black")

 g2 <- ggplot2::ggplot(data,
                       ggplot2::aes(x=age,group=period,y = outcome_var,col=period))+
   ggplot2::geom_point()+
   ggplot2::geom_path()+
   ggplot2::theme_bw()+
   ggplot2::labs(x = "Age Group",names = "Period",
                 y = as.character(outcome_var))+
   ggplot2::geom_point(data = data %>%
               dplyr::group_by(age) %>%
               dplyr::summarise(outcome_var = mean(outcome_var,na.rm = T)),
             mapping = ggplot2::aes(x = age,group=NA,
                                    y=outcome_var,col=NA),
             size = 3,shape=8,color="black")

 ggpubr::ggarrange(g1,g2,
            labels = c("A", "B"),
            ncol = 2, nrow = 1)
}


# combine ####
apci.plot <- function(model,
                           age,
                           period,
                           outcome_var,
                           type = "model",
                           quantile = NULL,
                           ...){
  if(type=="explore"){
  data <- as.data.frame(model$model$data)
  data$outcome_var <- data[,outcome_var]
  data$age <- data[,age]
  data$period <- data[,period]
  data <- dplyr::group_by(.data = data,age,period)
  data <- dplyr::summarise(.data=data,
                           outcome_var = mean(outcome_var,na.rm = T))
  # data <- data%>%
  #   group_by(age,period)%>%
  #   summarise(outcome_var = mean(outcome_var,na.rm = T))

  g1 <- ggplot2::ggplot(data,
        ggplot2::aes(x=period,group=age,y = outcome_var,col=age))+
    ggplot2::geom_point()+
    ggplot2::geom_path()+
    ggplot2::theme_bw()+
    ggplot2::labs(x = "Period Group",names = "Age",
                  y = as.character(outcome_var))+
    ggplot2::geom_point(data = data %>%
                dplyr::group_by(period) %>%
                dplyr::summarise(outcome_var = mean(outcome_var,na.rm = T)),
                mapping = ggplot2::aes(x = period,group=NA,y=outcome_var,col=NA),
              size = 3, shape=8,color="black")

  g2 <- ggplot2::ggplot(data,
            ggplot2::aes(x=age,group=period,y = outcome_var,col=period))+
    ggplot2::geom_point()+
    ggplot2::geom_path()+
    ggplot2::theme_bw()+
    ggplot2::labs(x = "Age Group",names = "Period",y = as.character(outcome_var))+
    ggplot2::geom_point(data = data %>%
                dplyr::group_by(age) %>%
                dplyr::summarise(outcome_var = mean(outcome_var,na.rm = T)),
                mapping = ggplot2::aes(x = age,
                                       group=NA,y=outcome_var,col=NA),
              size = 3,shape=8,color="black")

  g3 <- ggplot2::ggplot(data,
                        ggplot2::aes(x=period,y=age,fill=outcome_var))+
    ggplot2::geom_tile()+
    ggplot2::coord_equal()+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.title = ggplot2::element_blank())+
    ggplot2::labs(x = 'Period Group',
         y = 'Age Group')
  g4 <- ggplot2::ggplot(data,ggplot2::aes(x=age,y=outcome_var))+
    ggplot2::theme_void()+
    ggplot2::geom_text(x=0.5,y=0.5,
              label=c("Data Exploration"))

  g <- ggpubr::ggarrange(g3,g1+ggplot2::coord_flip(),g2,g4,
                    labels = c("C", "A","P"),
                    ncol = 2, nrow = 2)
  }

  if(type=="model"){
  g3 <- apci.plot.heatmap(model=model,
                          age = age,period = period,
                          quantile = quantile,
                          color_map = c('blue','yellow'))
  g3 <- g3+ggplot2::theme(legend.title = ggplot2::element_blank())+
    ggplot2::labs(caption = "")

  # define the scales
  data <- model$int_matrix
  data.raw <- as.data.frame(model$model$model)
  data.raw[,age] <- data.raw$acc
  data.raw[,period] <- data.raw$pcc


  data$period <- as.factor(rep(1:nlevels(data.raw[,period]),
                     each = nlevels(data.raw[,age])))
  data$age <- as.factor(
    rep(1:nlevels(data.raw[,age]),
        nlevels(data.raw[,period])) )
  data$value <- data$iaesti%>%as.character%>%as.numeric

  color_scale <- c(min(data$value,na.rm = T),
                   max(data$value,na.rm = T))
  color_scale[1] <- round(color_scale[1],2)-0.01
  color_scale[2] <- round(color_scale[2],2)+0.01

  bk <- seq(color_scale[1],color_scale[2],(color_scale[2]-color_scale[1])/5)
  # define the scales ===

  data <- model$age_effect%>%as.data.frame
  data$age_estimate <- data$age_estimate%>%as.character%>%as.numeric
  data$age_group <- data$age_group%>%as.character%>%as.numeric%>%as.factor

  g4 <- ggplot2::ggplot(data,
                        ggplot2::aes(x=age_group,group=NA,y = age_estimate))+
    ggplot2::theme_void()+
    ggplot2::geom_text(x=0.5,y=0.5,
              label=c("APC-I Model"))
  g5 <- ggplot2::ggplot(data,
                        ggplot2::aes(x=age_group,
                                     group=NA,y = age_estimate))+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::labs(x = "Age Group",
         y = "Estimated Age Effect")+
    ggplot2::theme_bw()

  data <- model$period_effect%>%as.data.frame
  data$period_estimate <- data$period_estimate%>%as.character%>%as.numeric
  data$period_group <- data$period_group%>%as.character%>%as.numeric%>%as.factor
  color_scale <- c(min(data$period_estimate,na.rm = T),
                   max(data$period_estimate,na.rm = T))
  color_scale[1] <- round(color_scale[1],2)-0.01
  color_scale[2] <- round(color_scale[2],2)+0.01

  g6 <- ggplot2::ggplot(data,
                        ggplot2::aes(x=period_group,group=NA,y = period_estimate))+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::labs(x = "Period Group",
         y = "Estimated Period Effect")+
    ggplot2::ylim(color_scale)+
    ggplot2::theme_bw()

  g <- ggpubr::ggarrange(g3,g5+ggplot2::coord_flip(),g6,g4,
                    labels = c("C", "A","P"),
                    ncol = 2, nrow = 2)
  }
  g
}

# barplot----
apci.bar <- function(model,
                     age,
                     period,
                     outcome_var,
                     cohort_label = NULL,
                     # type = "model",
                     # quantile = NULL,
                     ...){
  df <- as.data.frame(model$cohort_average)
  df$cohort_group <- if(is.null(cohort_label)){
    factor(df$cohort_group)
  }else{
    factor(df$cohort_group,labels = cohort_label)
  }
  df$group_id <- NA
  df$cohort_average <- as.numeric( as.character(df$cohort_average) )
  df$star <- ifelse(df$sig!="   ",df$cohort_average,NA)
  # df$cohort_average_exp <- exp(df$cohort_average)
  p <- ggplot2::ggplot(df,ggplot2::aes(group=group_id,y = cohort_average))+
    ggplot2::geom_bar(ggplot2::aes(x=cohort_group,fill=group_id),
                      stat="identity",
                      position=ggplot2::position_dodge(),
                      col="black")+
    ggplot2::scale_fill_brewer(palette="Greys",name = "Region")+
    ggplot2::theme_minimal()+
    ggplot2::labs(x = "cohort group",names = "",y = "cohort deviation")+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))+
    ggplot2::geom_text(ggplot2::aes(x=cohort_group,y=star*1.1),
                       label = "*",
              color="red",size = 7)
  p
}
# model <- APC_I
# age <- "acc"
# period <- "pcc"
#
# apci.bar(model = APC_I,
#         age = "acc",period = 'pcc')
