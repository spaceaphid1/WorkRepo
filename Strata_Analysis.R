#'Strave Analysis
#'Jackson Anderson
#'
#'
#'Question Addressing: are differences in strava times for a given segment significantly different than if the differences recorded were due to sampling (time recording) error alone?
#'
#' _Limitations/Contingencies_
#'
#'1) the distributions of times are not normal, so analysis will have to take that into account
#'2) The effort, or relative effort as strava calls it, will need to be taken into account, as there may be a relationship between this variable and the recorded time. 
#'
#'
#' _Variables/Hypotheses_
#' 
#' Null: There is no difference between expected accuracy error and observed accuracy error
#' 
#' _Methods for Analysis_
#' I will take the two fastest times of relative similar relative effort for the top 30 individuals of the 842 DH (corrected) segment at Lefthand Canyon as my observed sample set. I chose these as my samples because there is a good chance these individuals will have repeated this segment many times and their effort's on the trail are likely quite similar for their top 2 times (most are riding at a pro-level). I will then calculate the differences in these two times for each individual; if this difference is >=10 seconds, the sample will be discarded, while all others will be kept. I will then create a normal distribution based with mu = mean difference of observed samples and SD based on that of the observed samples. I will then see if the observed samples differ significantly from this theoretical distribution!