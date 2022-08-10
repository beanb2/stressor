from pycaret.regression import *
from pycaret.parallel import FugueBackend
exp_reg = setup(data = r.data, target = r.rr, n_jobs = 1)
models = compare_models(n_select = 9999)
# models = compare_models(n_select = 9999, parallel = FugueBackend("dask"))
