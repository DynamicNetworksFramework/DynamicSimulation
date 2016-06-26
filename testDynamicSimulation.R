


dy.data = prepare.dy.data(data,pdataincial)
cat("\n")

test_that("add.object function",{
  new.dy.data = trigger.change(dy.data,data,padd=1,pdelete=0)
  expect_gte(nrow(new.dy.data),nrow(dy.data))
  expect_lte(nrow(new.dy.data),ndata)
  expect_true(length(unique(new.dy.data[,1]))==nrow(new.dy.data))
})


test_that("delete.object function",{
  new.dy.data = trigger.change(dy.data,data,padd=0,pdelete=1)
  expect_lte(nrow(new.dy.data),nrow(dy.data))
  expect_gte(nrow(new.dy.data),1)
  expect_true(length(unique(new.dy.data[,1]))==nrow(new.dy.data))
})


for(i in 1:ndata){
  dy.data = trigger.change(dy.data,data)
  test_that("",{
    expect_lte(nrow(dy.data),ndata)
    expect_gte(nrow(dy.data),1)
    expect_true(length(unique(dy.data[,1]))==nrow(dy.data))
  })
}