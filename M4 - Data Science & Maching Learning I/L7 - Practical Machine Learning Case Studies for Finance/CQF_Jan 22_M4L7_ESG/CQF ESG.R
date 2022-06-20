library(pdftools)
library(tm)
library(vader)
library(ggplot2)

# Settings:
s_stopwords = c("annualreport")
setwd("C:/My Path")

#########

sh1 <- read.csv(file = "SASB Keywords.csv", head=TRUE, sep=",")	# Read SASB Keywords.
sh2 <- read.csv(file = "ESG_Reports.csv", head=FALSE, sep=",")	# Read names of the ESG company reports to analyse.

# Catch companies' names:
s_comp <- data.frame(1:nrow(sh2))		# Matrix #words x 1.
z1 <- gregexpr("_", sh2[ , 1])	# Determine position of 1st "_".
z2 <- gregexpr(",", sh2[ , 1])	# Determine position of 1st ",".
for(i in 1:nrow(sh2))
{	# Company name starts after the first "_" and ends before the 1st ",":
	s_comp[i, 1] <- substr(sh2[i, 1], z1[[i]][1] + 1, z2[[i]][1] - 1)
}

# Prepare calculation of relative word frequencies:
# Determine start & end index positions of SASB dimensions in keywords:
s_dd <- unique(sh1[ , 1])
s_dim <- matrix(NA, nrow = NROW(s_dd), ncol = 6)
colnames(s_dim) <- c("Dim", "Layer", "Start", "End", "Freq", "Valence")
s_dim <- data.frame(s_dim)
s_dim[ , "Dim"] <- rep(paste("Dim ", 1:NROW(s_dd), sep = ""), 1)	# Write vector with "Dim 1", "Dim 2", etc. for charting later.
s_dim[ , "Layer"] <- s_dd

# Find start & end positions of stem words:
for(i in 1:NROW(s_dd))
{	# Compare the SASB dimensions, like"Environment", "Social Capital", etc. with list of all keywords in sh1:
	zz <- which(sh1[, "Layer_1"] == s_dim[i, "Layer"])		# s_dd[i])
	s_dim[i, "Start"] <- min(zz)
	s_dim[i, "End"] <- max(zz)
}


x_reports <- NROW(sh2)	# Determine number of ESG reports we want to analyse.
w_count <- matrix(NA, nrow = 1, ncol = NROW(sh1))	# w_count holds the number of stem keywords that are matched to the text in the reports.
colnames(w_count) <- sh1[ , "Stem"]
w_doc <- list()
p_dim <- list()
# pp <- 1		# Set manually to, for example, 2 to analyse 1 report individually (=> "ESG_bluebay, 2019").
# If you want to analyse reports individually, set pp, then move to ENTRY POINT below.


for (pp in 1:x_reports)		# Start: large loop over number of reports.
{

########
# ENTRY POINT
########	
	x <- pdf_text(sh2[pp, 1])		# Vector x with #elements = #pages.
	x_dd <- pdf_data(sh2[pp, 1])		# Extract table format from report => helpful for detecting sentences in several columns.

	x_pages <- length(x_dd)		# Save number of pages for report pp.
	w_1 <- NULL		# XXX

	for (i in 1:x_pages)	# Loop through number of document pages.
	{

		x <- data.frame(x_dd[[i]][ , "text"])		# Matrix #words x 1 for page i of report..
# Determine number of sentences: there is a sentence when there are full stops (".") on page x:
		x_full_stops <- apply(x, 2, function(x) unlist(gregexpr("\\.", x)))	# Only elements with values > 0 have full stops.
		x_idx <- (1:NROW(x_full_stops) * (x_full_stops > 0))
		x_idx <- x_idx[x_idx > 0]	# x_idx comprises the indices with words and full stops, i.e., the last word in a sentence.

		{
		if(length(x_idx) == 0)
			{		# No full stops (".") on this page.
			x_sent <- matrix(0, nrow = 1, ncol = ncol(w_count))	# XXX
			s_sent <- data.frame(Vader = NA, Stc_Raw = NA, Stc_Stem = NA, stringsAsFactors = FALSE)	# s_sent holds all sentences of page i.
			}
		else
			{
				s_sent <- data.frame(Vader = 1:NROW(x_idx), Stc_Raw = 1:NROW(x_idx), Stc_Stem = 1:NROW(x_idx), stringsAsFactors = FALSE)	# s_sent holds all sentences of page i.

				x_1 <- 1
				x_sent <- matrix(NA, nrow = NROW(x_idx), ncol = ncol(w_count))

				for (j in 1:NROW(x_idx))	# NROW(x_idx)	# Loop through number of sentences.
# Goal is to concatenate the consecutive elements of x to 1 sentence.
				{
					s <- x[x_1:x_idx[j], 1]		# Each element of s holds 1 word of the sentence.
					s_clean <- gsub("[[:punct:]]", "", s)		# Remove all punctuation.
					s_clean <- removeWords(s_clean, s_stopwords)	# Remove any stopwords.
					s_stem <- stemDocument(s_clean, language = "english")	# Create stem words for sentence.

# Convert the part of the vector to list:
					y <- list()
					y2 <- y

					for (qq in 1:NROW(s))
					{	# To merge the individual words to a whole sentence, transfer the elements of s_stem/s_clean to a list.
						y[[qq]] <- s_stem[qq]		# s_clean[qq]
						y2[[qq]] <- s_clean[qq]
					}

					s_sent[j, "Stc_Raw"] <- do.call(paste, y2)	#  This is the whole sentence, cleansed.
					s_sent[j, "Stc_Stem"] <- do.call(paste, y)	# This is the whole sentence, cleansed and transfered to stem words.
					zz <- get_vader(s_sent[j, "Stc_Stem"])
					s_sent[j, "Vader"] <- zz["compound"]	# Save the compund Vader sentiment score.

					x_count <- numeric(ncol(w_count))	# Initialise word counter for sentence j with 0s.	# matrix(NA, nrow = 1, ncol = ncol(w_count))

					for (qq in 1:NROW(s_stem))	# Loop through the number words in s_stem.
					{
						zz <- match(s_stem[qq], colnames(w_count))
						if(!is.na(zz))
						{
							x_count[zz] <- x_count[zz] + 1	# This keyword is a match => increase counter by 1.
						}
					}

					x_sent[j, ] <- x_count		# Save counts of words in each sentence.
	
					x_1 <- 1 + x_idx[j]	# Increase counter to read first word in sentence.

				}	# for (j in 1:NROW(x_idx)).
			}	# End: else.
	

		}	# end: if(length(x_idx) == 0).

		x_1 <- rep(i, NROW(x_sent))	# Set page number in document.
		x_2 <- rep(NA, NROW(x_sent))	# "no_sent_consec" => AMEND LATER!!! XXX
		x_3 <- 1:NROW(x_sent)		# Set sentence number on page i.
		w_1 <- rbind(w_1, cbind(x_1, x_2, x_3, x_sent, s_sent))	# Result matrix: combine #page, #sentence and each senetence as string with keyword count.
		w_doc[[pp]] <- w_1		# Save result matrix for each document: w_doc holds 1 result matrix for each report.

# s_sent now holds the sentences on page i: each element of s_sent comprises 1 sentence.

		print(paste("Company:" , s_comp[pp, 1], ", Page:", i), sep ="")	# Print status line.

	}	# End: # Loop through number of document pages.


	colnames(w_1) <- c("no_page", "no_sent_consec", "no_sent_on_page", colnames(w_count), colnames(s_sent))

	x_col <- match("Vader", colnames(w_1)) - 1	# Determine column index of "Vader" in result matrix.
	w_dd <- w_1[ , 4:x_col]
	x_1 <- apply(w_dd, 1, sum)	# Determine sentences with at least 1 valid term, these would have row sum > 0.
	x_2 <- 1 * (x_1 > 0)	# x_2 holds the sentences with valid terms.
	x_vader <- as.numeric(w_1[ , "Vader"])	# x_vader holds the valence values for each sentence.
	x_vader[is.na(x_vader)] <- 0		# Set all NAs to 0.

	x_vader_avg <- x_2 %*% x_vader / sum(x_2)		# Avg. valence score for the document, based on all sentences with valid terms (SASB relevant V).
	w_each_sent <- 1 / sum(x_2)	# Each sentence is equal weighted to calculate document valence.

	w_2 <- w_each_sent * x_vader * (w_dd / x_1)		# (w_avg per sent.) * valence(sentence_i) * (term_matrix / #terms in sent.)
	w_2[is.na(w_2)] <- 0		# Set all NAs to 0.
# Result is a weighted term matrix (#sentences x #terms) that can be used to examine the valence contributions of individual terms (sum of 1 column) as well as (sub-) dimensions (sum of columns belonging to 1 dim.).

# Calculate SASB term frequencies (here only for the 5 main dimensions. This can be easily extended to also included the 2nd layer):

	x_offset <- match(sh1[1, "Stem"], colnames(w_1)) - 1		# Find 1st stem word in w_1 => we need this as column offset.

	for (i in 1:nrow(s_dim))	# Loop through # dimensions:
	{
# Term frequencies for SASB dimensions:
		s_dim[i, "Freq"] <- sum(w_1[ , x_offset + s_dim[i, "Start"]:s_dim[i, "End"]]) / sum(w_1[ , x_offset + s_dim[1, "Start"]:s_dim[nrow(s_dim), "End"]])
# Valence for SASB dimensions:
		s_dim[i, "Valence"] <- sum(w_2[ , s_dim[i, "Start"]:s_dim[i, "End"]])
	}

	p_dim[[pp]] <- s_dim	# Write results to list.


#########
# EXIT POINT.
#########

}		# End: for (pp in 1:x_reports).


# Prepare graphical display of results:
# 1) Term frequencies:
m <- matrix(NA, nrow = NROW(s_comp) * NROW(s_dim), 3)
colnames(m) <- c("Company", "Layer", "Freq")
df <- data.frame(m)
m <- matrix(NA, nrow = NROW(s_comp) * (NROW(s_dim) + 1), 3)
colnames(m) <- c("Company", "Layer", "Valence")
df_2 <- data.frame(m)

for (i in 1:x_reports)
{
	df[(1 + (i - 1) * NROW(s_dim)):(i * NROW(s_dim)), "Company"] <- s_comp[i, 1]
	df[(1 + (i - 1) * NROW(s_dim)):(i * NROW(s_dim)), "Layer"] <- substr(p_dim[[i]][ , "Layer"], 1, 10)
	df[(1 + (i - 1) * NROW(s_dim)):(i * NROW(s_dim)), "Freq"] <- p_dim[[i]][ , "Freq"]

	df_2[(1 + (i - 1) * (NROW(s_dim) + 1)):(i * (NROW(s_dim) + 1)), "Company"] <- s_comp[i, 1]
	df_2[(1 + (i - 1) * (NROW(s_dim) + 1)):(i * (NROW(s_dim) + 1)), "Layer"] <- substr(c(p_dim[[i]][ , "Layer"], "Total"), 1, 10)
	x_vader_total <- sum(p_dim[[i]][ , "Valence"])
	df_2[(1 + (i - 1) * (NROW(s_dim) + 1)):(i * (NROW(s_dim) + 1)), "Valence"] <- c(p_dim[[i]][ , "Valence"], x_vader_total)
}

# Plot term frequencies (Freq):
ggplot(df, aes(Layer, Company, fill = Freq)) + geom_tile() + ggtitle("Frequencies") + theme(plot.title = element_text(hjust = 0.5, lineheight=1.3, face="bold")) + theme(axis.text.x = element_text(angle = 90))

# 2) Valence:
ggplot(df_2, aes(Layer, Company, fill = Valence)) + geom_tile() + ggtitle("Valence") + theme(plot.title = element_text(hjust = 0.5, lineheight=1.3, face="bold")) + theme(axis.text.x = element_text(angle = 90, face="bold")) + scale_fill_gradient(low="blue", high="red") 

