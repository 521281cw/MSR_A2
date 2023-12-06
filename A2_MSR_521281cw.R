library (MSR)

## Q1

# estimate the Bass parameters of each app

bass.dogbook <- estimate_bass(app.installs$Dogbook)
bass.dogbook

bass.books <- estimate_bass(app.installs$Books)
bass.books

bass.puzzlebee <- estimate_bass(app.installs$PuzzleBee)
bass.puzzlebee

bass.astrology <- estimate_bass(app.installs$Astrology)
bass.astrology

bass.languages <- estimate_bass(app.installs$Languages)
bass.languages

bass.biorhytms <-estimate_bass(app.installs$BioRhythms)
bass.biorhytms

bass.tvshows <- estimate_bass(app.installs$`TV Shows`)
bass.tvshows

bass.greekcommunity <- estimate_bass(app.installs$`The Greek Community`)
bass.greekcommunity

bass.popculture <- estimate_bass(app.installs$`Pop Culture Quizzes`)
bass.popculture

bass.countdowns <- estimate_bass(app.installs$`My Countdowns`)
bass.countdowns

bass.yomamma <- estimate_bass(app.installs$`Yo Momma`)
bass.yomamma

bass.fantasystock <- estimate_bass(app.installs$`Fantasy Stock Exchange`)
bass.fantasystock

bass.personaldna <- estimate_bass(app.installs$PersonalDNA)
bass.personaldna

bass.weather <- estimate_bass(app.installs$Weather)
bass.weather

bass.flufffriends <- estimate_bass(app.installs$`(fluff)Friends`)
bass.flufffriends

## Predicting cumulative installations
T <- 181:200
N_Chess <- predict_bass(T, (c(0.0017119455, 0.01060018, 243089010)))
N_Chess


## Q2


# Selecting distance measure
spotify_dist <- dist(spotify[1:6], method = "euclidean")

# Selecting clustering procedure
results_spotify <- hclust(spotify_dist, method =  "ward.D2")
results_spotify

# Determining number of clusters
within_cluster_variation_spotify <- results_spotify$height
within_cluster_variation_spotify

elbow_plot(within_cluster_variation_spotify)

# 5 clusters is optimal

# ANOVA

clust_5 <- cutree(results_spotify,5)
clust_5

clust_5 <- as.factor(clust_5)

anova_spotify <- aov(cbind(acousticness,
                       danceability,
                       energy,
                       instrumentalness,
                       speechiness,
                       valence,
                       ID) ~ clust_5, data = spotify)
summary(anova_spotify)


cluster_mean(anova_spotify)
