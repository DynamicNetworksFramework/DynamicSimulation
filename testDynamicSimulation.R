


dy.data = prepare.dy.data(ndata,pdataincial)
cat("\n")

test_that("add.object function",{
  new.dy.data = trigger.change(dy.data,ndata,padd=1,pdelete=0)
  expect_gte(nrow(new.dy.data),nrow(dy.data))
  expect_lte(nrow(new.dy.data),ndata)
  expect_true(length(unique(new.dy.data))==nrow(new.dy.data))
})


test_that("delete.object function",{
  new.dy.data = trigger.change(dy.data,ndata,padd=0,pdelete=1)
  expect_lte(nrow(new.dy.data),nrow(dy.data))
  expect_gte(nrow(new.dy.data),1)
  expect_true(length(unique(new.dy.data))==nrow(new.dy.data))
})


for(i in 1:ndata){
  dy.data = trigger.change(dy.data,ndata)
  test_that("",{
    expect_lte(nrow(dy.data),ndata)
    expect_gte(nrow(dy.data),1)
    expect_true(length(unique(dy.data))==nrow(dy.data))
  })
}