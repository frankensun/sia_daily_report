analyse_category <- function(filename) {
    analyse_data = read.csv(filename)
    dist_data = dist(analyse_data, method='eucldean')
    clust_data = hclust(dist_data, method='ward')
    cut_data = cuttree(clust_data, k=3)
    as.data.frame(cut_data)
}
