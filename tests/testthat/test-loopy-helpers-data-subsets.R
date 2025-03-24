test_that("extract data subsets - no postprocessing", {
	skip_if_not_installed("modeldata")

	data("two_class_dat", package = "modeldata")
	two_class_rs <- mc_cv(two_class_dat, times = 2)
	rs_split <- two_class_rs$splits[[1]]

	wflow_1 <- workflow(Class ~ ., dt_spec)
	data_1 <- melodie:::get_data_subsets(wflow_1, rs_split)
	expect_named(data_1, c("data_perf", "ind_perf", "data_fit", "ind_fit"))
	expect_equal(data_1$data_fit, analysis(rs_split))
	expect_equal(data_1$data_perf, assessment(rs_split))
	expect_equal(data_1$ind_fit, as.integer(rs_split))
	expect_equal(data_1$ind_perf, as.integer(rs_split, data = "assessment"))
})

test_that("extract data subsets - no estimated postprocessing", {
	skip_if_not_installed("modeldata")

	data("two_class_dat", package = "modeldata")
	two_class_rs <- mc_cv(two_class_dat, times = 2)
	rs_split <- two_class_rs$splits[[1]]

	wflow_1 <- workflow(Class ~ ., dt_spec, cls_post)
	data_1 <- melodie:::get_data_subsets(wflow_1, rs_split)
	expect_named(data_1, c("data_perf", "ind_perf", "data_fit", "ind_fit"))
	expect_equal(data_1$data_fit, analysis(rs_split))
	expect_equal(data_1$data_perf, assessment(rs_split))
	expect_equal(data_1$ind_fit, as.integer(rs_split))
	expect_equal(data_1$ind_perf, as.integer(rs_split, data = "assessment"))
})

test_that("extract data subsets - estimated postprocessing", {
	skip_if_not_installed("modeldata")

	data("two_class_dat", package = "modeldata")
	two_class_rs <- mc_cv(two_class_dat, times = 2)
	mc_cv_args <- rsample::.get_split_args(two_class_rs)

	rs_split <- two_class_rs$splits[[1]]

	set.seed(1)
	rs_sub_split <- rsample::inner_split(rs_split, mc_cv_args)

	wflow_1 <- workflow(Class ~ ., dt_spec, cls_est_post)

	set.seed(1)
	data_1 <- melodie:::get_data_subsets(wflow_1, rs_split, mc_cv_args)

	expect_named(
		data_1,
		c("data_perf", "ind_perf", "ind_cal", "data_cal", "data_fit", "ind_fit")
	)
	expect_equal(data_1$data_fit, analysis(rs_sub_split))
	expect_equal(data_1$data_cal, assessment(rs_sub_split))
	expect_equal(data_1$data_perf, assessment(rs_split))

	expect_equal(data_1$ind_fit, as.integer(rs_sub_split))
	expect_equal(data_1$ind_cal, as.integer(rs_sub_split, data = "assessment"))
	expect_equal(data_1$ind_perf, as.integer(rs_split, data = "assessment"))
})
