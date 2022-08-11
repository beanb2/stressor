from pycaret.classification import *
refit_mlm_new_model = r.refit_mlm_temp.fit(r.refit_mlm_X, r.refit_mlm_y)
predictions = predict_model(refit_mlm_new_model, data = r.refit_mlm_test)
