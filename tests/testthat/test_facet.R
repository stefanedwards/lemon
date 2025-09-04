library(vdiffr)

test_that('We can reproduce issue 24', {
  set.seed(123)
  est=rnorm(6, sd=2)
  dd = data.frame(lev=rep(c("Level 1", "Level 2", "Level 3"), 2),
                  pos = rep(c("Pos 1", "Pos 2"), each=3),
                  est,
                  ciLow = est-abs(rnorm(6, 1, sd=3)),
                  ciHigh = est+abs(rnorm(6, 1, sd=3))
  )

  p = expect_warning(
    ggplot(dd, aes(x=lev, y=est)) +
      geom_point(shape=1) +
      facet_rep_grid(~pos) +
      geom_errorbar(aes(ymin=ciLow, ymax=ciHigh), width=0) +
      coord_flip() + theme_bw() +
      theme(panel.border =  element_blank(), axis.line = element_line()),
     "soft-deprecated", fixed=TRUE)

  expect_doppelganger('facet_rep_grid spacing', p)

  p = expect_warning(
    ggplot(dd, aes(x=est, y=pos)) +
      geom_point(shape=1) +
      facet_rep_wrap(~lev,ncol=1) +
      geom_errorbar(aes(ymin=ciLow, ymax=ciHigh), width=0) +
      coord_flip() + theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)),
    "soft-deprecated", fixed=TRUE)
  expect_doppelganger('facet_rep_wrap spacing', p)
})


test_that('We can reproduce issue 24, round 2', {
  set.seed(123)
  est=rnorm(6, sd=2)
  dd = data.frame(lev=rep(c("Level 1", "Level 2", "Level 3"), 2),
                  pos = rep(c("Posxxxxxxxxxxx 1", "Pos 2"), each=3),
                  est,
                  ciLow = est-abs(rnorm(6, 1, sd=3)),
                  ciHigh = est+abs(rnorm(6, 1, sd=3))
  )
  
  p = ggplot(dd, aes(x=est, y=pos))
  p = p + geom_point(shape=1)
  p = expect_warning(
    p + facet_rep_wrap(~lev,ncol=1),
    "soft-deprecated", fixed=TRUE)
  p = p + geom_errorbar(aes(ymin=ciLow, ymax=ciHigh), width=0)
  p = p + coord_flip() + theme_bw()
  p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                           vjust = 0.5))
  expect_doppelganger('facet_rep_wrap spacing-2', p)
})
