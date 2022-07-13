from pycaret.regression import *
exp_reg = setup(data = r.data, target = 'Y', n_jobs = 1)
models = compare_models(n_select = 50)
