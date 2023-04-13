from pycaret.classification import *
refit_mlm_new_model = r.refit_mlm_temp.fit(r.refit_mlm_X, r.refit_mlm_y)
predictions = refit_mlm_new_model.predict(r.refit_mlm_test)
