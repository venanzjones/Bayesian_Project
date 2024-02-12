import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

class PostPred:
    def __init__(self, posterior_az, Y, test_train = False, idx_test = None):
        self.Y_test = None
        self.posterior_az = posterior_az
        self.posterior = posterior_az.posterior
        self.posterior_med = self.posterior.median(dim=['chain', 'draw'])

        self.idx_obs = Y[Y.notna()].index
        self.idx_miss = Y[Y.isna()].index

        if test_train:
            self.idx_test = idx_test
            self.Y_test = Y[idx_test].reset_index(drop=True)
            self.idx_obs = sorted(np.setdiff1d(self.idx_obs, self.idx_test))
            self.idx_miss = sorted(np.concatenate([self.idx_miss, self.idx_test]))

        self.Y_miss = Y[self.idx_miss].reset_index(drop=True)
        self.Y_obs = Y[self.idx_obs].reset_index(drop=True)

        self.Y = np.array(Y)
        if 'y_pred_miss' in self.posterior:
            self.Y[Y.isna()] = np.nan      
        else:
            self.Y = self.Y[self.idx_obs]
        

    def predict(self, use_mean=False, CI=False, alpha=0.05, error_metrics=False):
        if CI:
            quantiles = [0.5, alpha/2, 1-alpha/2]
        else:
            quantiles = [0.5]

        if use_mean:
            y_pred_obs = self.posterior.y_pred.mean(dim=['chain', 'draw']).T
            y_pred_obs = pd.DataFrame(y_pred_obs.values, columns=['pred'], index = self.idx_obs)
            if CI:
                y_pred_obs_CI = self.posterior.y_pred.quantile(quantiles[1, 2], dim=['chain', 'draw']).T
                y_pred_obs_CI = pd.DataFrame(y_pred_obs_CI.values, columns=[f'{alpha/2}', f'{1-alpha/2}'], index = self.idx_obs)
                y_pred_obs = pd.concat([y_pred_obs, y_pred_obs_CI], axis=1)
        else:
            y_pred_obs = self.posterior.y_pred.quantile(quantiles, dim=['chain', 'draw']).T
            y_pred_obs = pd.DataFrame(y_pred_obs.values, index = self.idx_obs)
            if CI:
                y_pred_obs.columns = ['pred', f'{alpha/2}', f'{1-alpha/2}']
            else:
                y_pred_obs.columns = ['pred']

        if 'y_pred_miss' in self.posterior:
            if use_mean:
                y_pred_miss = self.posterior.y_pred_miss.mean(dim=['chain', 'draw']).T
                y_pred_miss = pd.DataFrame(y_pred_obs.values, columns=['pred'], index = self.idx_miss)
                if CI:
                    y_pred_miss_CI = self.posterior.y_pred_miss.quantile(quantiles[1, 2], dim=['chain', 'draw']).T
                    y_pred_miss_CI = pd.DataFrame(y_pred_obs_CI.values, columns=[f'{alpha/2}', f'{1-alpha/2}'], index = self.idx_miss)
                    y_pred_miss = pd.concat([y_pred_miss, y_pred_miss_CI], axis=1)
            else:
                y_pred_miss = self.posterior.y_pred_miss.quantile(quantiles, dim=['chain', 'draw']).T
                y_pred_miss = pd.DataFrame(y_pred_miss.values, index = self.idx_miss)
                if CI:
                    y_pred_miss.columns = ['pred', f'{alpha/2}', f'{1-alpha/2}']
                else:
                    y_pred_miss.columns = ['pred']
            y_pred = pd.concat([y_pred_obs, y_pred_miss], axis=0).sort_index()
        else:
            y_pred = y_pred_obs.reset_index(drop=True)

        if error_metrics:
            if self.Y_test is not None and 'y_pred_miss' in self.posterior:
                idx_obs_test = sorted(np.concatenate([self.idx_obs, self.idx_test]))
                y_star = y_pred['pred'][idx_obs_test].reset_index(drop=True)
                residuals = y_star - self.Y[idx_obs_test]
                y_star_test = y_pred['pred'][self.idx_test].reset_index(drop=True)
                residuals_test = y_star_test - self.Y[self.idx_test]
            else:
                y_star = y_pred_obs['pred'].reset_index(drop=True)
                residuals = y_star - self.Y_obs

            mse = np.mean(residuals**2)
            mae = np.mean(np.abs(residuals))
            mad = np.median(np.abs(residuals))

            metrics = {
                'y_obs': y_star,
                'residuals': residuals,
                'mse': mse,
                'mae': mae,
                'mad': mad,
            }
            if self.Y_test is not None and 'y_pred_miss' in self.posterior:
                mse_test = np.mean(residuals_test**2)
                mae_test = np.mean(np.abs(residuals_test))
                mad_test = np.median(np.abs(residuals_test))
                metrics['mse_test'] = mse_test
                metrics['mae_test'] = mae_test
                metrics['mad_test'] = mad_test

            if CI:
                if self.Y_test is not None and 'y_pred_miss' in self.posterior:
                    idx_obs_test = sorted(np.concatenate([self.idx_obs, self.idx_test]))
                    y_star_up = y_pred[f'{1-alpha/2}'][idx_obs_test].reset_index(drop=True)
                    y_star_low = y_pred[f'{alpha/2}'][idx_obs_test].reset_index(drop=True)
                    outliers = np.where((self.Y[idx_obs_test] > y_star_up) | (self.Y[idx_obs_test] < y_star_low))[0]
                    y_star_up_test = y_pred[f'{1-alpha/2}'][self.idx_test].reset_index(drop=True)
                    y_star_low_test = y_pred[f'{alpha/2}'][self.idx_test].reset_index(drop=True)
                    outliers_test = np.where((self.Y[self.idx_test] > y_star_up_test) | (self.Y[self.idx_test] < y_star_low_test))[0]
                    metrics['outliers_test'] = outliers_test
                    percentage_inside = 1 - len(outliers)/len(self.Y[idx_obs_test])
                    percentage_inside_test = 1 - len(outliers_test)/len(self.Y[self.idx_test])
                    metrics['percentage_inside_CI_test'] = percentage_inside_test
                else:
                    y_star_up = y_pred_obs[f'{1-alpha/2}'].reset_index(drop=True)
                    y_star_low = y_pred_obs[f'{alpha/2}'].reset_index(drop=True)
                    outliers = np.where((self.Y_obs > y_star_up) | (self.Y_obs < y_star_low))[0]
                    percentage_inside = 1 - len(outliers)/len(self.Y_obs)
                metrics['outliers'] = outliers
                metrics['percentage_inside_CI'] = percentage_inside

            return y_pred, metrics

        return y_pred
    
    
            
            

