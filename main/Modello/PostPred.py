import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

class PostPred:
    def __init__(self, posterior_az, Y):
        self.posterior_az = posterior_az
        self.posterior = posterior_az.posterior
        self.posterior_med = self.posterior.median(dim=['chain', 'draw'])

        self.Y = np.array(Y)

        Y_miss = Y[Y.isna()]
        Y_obs = Y[Y.notna()]

        self.Y_obs_index = Y_obs.index
        self.Y_miss_index = Y_miss.index

        self.Y_miss = Y_miss.reset_index(drop=True)
        self.Y_obs = Y_obs.reset_index(drop=True)

        self.Y[self.Y_miss_index] = np.nan
            

    def predict(self, use_mean=False, CI=False, alpha=0.05, error_metrics=False):
        if CI:
            quantiles = [0.5, alpha/2, 1-alpha/2]
        else:
            quantiles = [0.5]

        if use_mean:
            y_pred_obs = self.posterior.y_pred.mean(dim=['chain', 'draw']).T
            y_pred_obs = pd.DataFrame(y_pred_obs.values, columns=['pred'], index = self.Y_obs_index)
            if CI:
                y_pred_obs_CI = self.posterior.y_pred.quantile(quantiles, dim=['chain', 'draw']).T
                y_pred_obs_CI = pd.DataFrame(y_pred_obs_CI.values, columns=[f'{alpha/2}', f'{1-alpha/2}'], index = self.Y_obs_index)
                y_pred_obs = pd.concat([y_pred_obs, y_pred_obs_CI], axis=1)
        else:
            y_pred_obs = self.posterior.y_pred.quantile(quantiles, dim=['chain', 'draw']).T
            y_pred_obs = pd.DataFrame(y_pred_obs.values, columns=['pred', f'{alpha/2}', f'{1-alpha/2}'], index = self.Y_obs_index)

        if 'y_pred_miss' in self.posterior:
            if use_mean:
                y_pred_miss = self.posterior.y_pred_miss.mean(dim=['chain', 'draw']).T
                y_pred_miss = pd.DataFrame(y_pred_obs.values, columns=['pred'], index = self.Y_miss_index)
                if CI:
                    y_pred_miss_CI = self.posterior.y_pred_miss.quantile(quantiles, dim=['chain', 'draw']).T
                    y_pred_miss_CI = pd.DataFrame(y_pred_obs_CI.values, columns=[f'{alpha/2}', f'{1-alpha/2}'], index = self.Y_miss_index)
                    y_pred_miss = pd.concat([y_pred_miss, y_pred_miss_CI], axis=1)
            else:
                y_pred_miss = self.posterior.y_pred_miss.quantile(quantiles, dim=['chain', 'draw']).T
                y_pred_miss = pd.DataFrame(y_pred_miss.values, columns=['pred', f'{alpha/2}', f'{1-alpha/2}'], index = self.Y_miss_index)
            y_pred = pd.concat([y_pred_obs, y_pred_miss], axis=0).sort_index()
        else:
            y_pred = y_pred_obs.reset_index(drop=True)

        if error_metrics:
            y_star = y_pred_obs['pred']
            y_star_up = y_pred_obs[f'{1-alpha/2}']
            y_star_low = y_pred_obs[f'{alpha/2}']

            y_star = y_star.reset_index(drop=True)
            y_star_up = y_star_up.reset_index(drop=True)
            y_star_low = y_star_low.reset_index(drop=True)

            outliers = np.where((self.Y_obs > y_star_up) | (self.Y_obs < y_star_low))[0]
            percentage_inside = 1 - len(outliers)/len(self.Y_obs)

            residuals = y_star - self.Y_obs
            mse = np.mean(residuals**2)
            mae = np.mean(np.abs(residuals))

            median_predictions = self.posterior.y_pred.median(dim=["chain", "draw"])
            absolute_deviations = np.abs(self.Y_obs - median_predictions)
            mad_median = np.mean(absolute_deviations).item()

            metrics = {
                'y_obs': y_star,
                'outliers': outliers,
                'residuals': residuals,
                'mse': mse,
                'mae': mae,
                'mad_median': mad_median,
                'percentage_inside_CI': percentage_inside
            }

            return y_pred, metrics

        return y_pred
    
    
            
            

