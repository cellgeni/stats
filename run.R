
library(cowplot)

tbl_sh <- read.table("data/sharing.txt")
colnames(tbl_sh) <- c("month", "day", "path", "bytes", "giga", "nsamp")

sharing_by_giga = aggregate(tbl_sh$giga, by=list(month=tbl_sh$month), FUN=sum)
sharing_by_samp = aggregate(tbl_sh$nsamp, by=list(month=tbl_sh$month), FUN=sum)

sharing_by_samp$x <- cumsum(sharing_by_samp$x)
sharing_by_giga$x <- cumsum(sharing_by_giga$x)

months=c("May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov")

p_sharing_tb <-
  ggplot(sharing_by_giga, aes(x=as.factor(month), y=x/1000, group=1)) + geom_line() + geom_point() +
    scale_x_discrete(breaks=5:11, labels=months) +
    xlab(NULL) + ylab("TB")

p_sharing_nsample <-
  ggplot(sharing_by_samp, aes(x=as.factor(month), y=x, group=1)) + geom_line() + geom_point() +
    scale_x_discrete(breaks=5:11, labels=months) +
    xlab(NULL) + ylab("# samples")

plot_grid(p_sharing_nsample, p_sharing_tb, nrow = 1)
ggsave("pdf/sharing.pdf", w = 9, h = 3)
ggsave("png/sharing.png", w = 9, h = 3)

#
 # #
  #  #  #
#  #  #  #  #
  #  #  #
 # #
#


tbl_bs <- read.table("data/bulkseq.txt")
colnames(tbl_bs) <- c("month", "day", "path", "nsamp")

bulkseq_by_samp = aggregate(tbl_bs$nsamp, by=list(month=tbl_bs$month), FUN=sum)

bulkseq_by_samp$x <- cumsum(bulkseq_by_samp$x)

p_bulkseq_nsample <-
  ggplot(bulkseq_by_samp, aes(x=as.factor(month), y=x, group=1)) + geom_line() + geom_point() +
    scale_x_discrete(breaks=5:11, labels=months) +
    xlab(NULL) + ylab("# samples") + ggtitle("Bulk RNAseq")

#
 # #
  #  #  #
#  #  #  #  #
  #  #  #
 # #
#


tbl_sc <- read.table("data/singlecell.txt")
colnames(tbl_sc) <- c("month", "day", "path", "nsamp")

singlecell_by_samp = aggregate(tbl_sc$nsamp, by=list(month=tbl_sc$month), FUN=sum)

singlecell_by_samp$x <- cumsum(singlecell_by_samp$x)

p_singlecell_nsample <-
  ggplot(singlecell_by_samp, aes(x=as.factor(month), y=x, group=1)) + geom_line() + geom_point() +
    scale_x_discrete(breaks=5:11, labels=months) +
    xlab(NULL) + ylab("# cells") + ggtitle("Single Cell RNAseq")


plot_grid(p_bulkseq_nsample, p_singlecell_nsample, nrow = 1)
ggsave("pdf/rnaseq.pdf", w = 9, h = 3)
ggsave("png/rnaseq.png", w = 9, h = 3)
