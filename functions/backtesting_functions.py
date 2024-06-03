import pandas as pd
import numpy as np

from rds2py import read_rds
from sklearn.metrics import mean_absolute_error, mean_squared_error

from datetime import datetime
import os
import math

import warnings
warnings.filterwarnings("ignore", category=FutureWarning)
warnings.filterwarnings("ignore", category=RuntimeWarning)

def get_position(df):
    df['position'] = np.nan
    for i in range(len(df) - 1):  
        
        if i == 0: 
            df['position'].iloc[i] = 1

        else:
            if np.sign(df['forecast'].iloc[i+1])==np.sign(df['forecast'].iloc[i]):
                df['position'].iloc[i] = df['position'].iloc[i-1]

            elif np.sign(df['forecast'].iloc[i + 1])==1:
                df['position'].iloc[i] = 1
            
            elif np.sign(df['forecast'].iloc[i + 1])==-1:
                df['position'].iloc[i] = -1
    
    return df

def mae(actual_values, predicted_values):
    mae = mean_absolute_error(actual_values, predicted_values)
    return mae

def mse(actual_values, predicted_values):
    mse = mean_squared_error(actual_values, predicted_values)
    return mse

def rmse(actual_values, predicted_values):
    rmse = np.sqrt(mean_squared_error(actual_values, predicted_values))
    return rmse

def mean_absolute_percentage_error(y_true, y_pred):
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100

def EquityCurve_na_StopyZwrotu(tab):

    ret=[]

    for i in range(0,len(tab)-1):
        ret.append((tab[i+1]/tab[i])-1)

    return ret

def ARC(tab):
    temp=EquityCurve_na_StopyZwrotu(tab)
    lenth = len(tab)
    a_rtn=1
    for i in range(len(temp)-1):
        rtn=(1+temp[i])
        a_rtn=a_rtn*rtn
    if a_rtn <= 0:
        a_rtn = 0
    else:
        a_rtn = math.pow(a_rtn,(252/lenth)) - 1
    return 100*a_rtn

def MaximumDrawdown(tab):

    eqr = np.array(EquityCurve_na_StopyZwrotu(tab))
    
    cum_returns = np.cumprod(1 + eqr)
    cum_max_returns = np.maximum.accumulate(cum_returns)
    drawdowns = (cum_max_returns - cum_returns) / cum_max_returns
    max_drawdown = np.max(drawdowns)
    return max_drawdown*100

def ASD(tab):
    return ((((252)**(1/2)))*np.std(EquityCurve_na_StopyZwrotu(tab)))*100

def IR1(tab):
    aSD = ASD(tab)
    ret = ARC(tab)
    mianownik = aSD
    if mianownik == 0:
        return 0
    else: 
        return ret / mianownik
    
def sgn(x):
    if x==0:
        return 0
    else:
        return int(abs(x)/x)

def IR2(tab):
    aSD = ASD(tab)
    ret = ARC(tab)
    md = MaximumDrawdown(tab)
    mianownik = aSD * md
    if mianownik == 0:
        return 0
    else: 
        licznik = ret ** 2 * sgn(ret)
        return licznik / mianownik

def cases_df(FORECASTS_BC_SA, df_SnP500_lr, case_1):
    df_case_strat = pd.DataFrame(columns=case_1, index=FORECASTS_BC_SA['arima-garch_base_case'].index)

    for strat in case_1:

        res = pd.DataFrame()

        res['forecast'] = FORECASTS_BC_SA[strat]
        res['SnP500_lr'] = df_SnP500_lr.loc[FORECASTS_BC_SA['arima-garch_base_case'].index]
        res['SnP500_lr'] = np.exp(res['SnP500_lr'])-1 # converting to simple returns

        res = get_position(res)
        res = res[~res.index.duplicated(keep=False)]

        strategy = (res['SnP500_lr'] * res['position'].shift(1))
        strategy = (1+strategy.fillna(0)).cumprod()
        
        df_case_strat[strat] = strategy
        df_case_strat[f'{strat}_forecast'] = res['forecast']

        if strat == 'arima-garch_base_case':
            df_case_strat['buy_n_hold'] = (1 + res['SnP500_lr'].fillna(0)).cumprod()
            df_case_strat['buy_n_hold_forecast'] = res['SnP500_lr']
        
    case_1.append('buy_n_hold')

    return df_case_strat, case_1

def performance_metrics_case_1(STRING_1, STRING_2, df, case):

    buy_n_hold = df[case[-1]]
    buy_n_hold_forecast = df[f'{case[-1]}_forecast']

    strat_1 = df[case[0]]
    strat_1_forecast = df[f'{case[0]}_forecast']

    strat_2 = df[case[1]]
    strat_2_forecast = df[f'{case[1]}_forecast']
    
    df_data = {

    "": ["S&P 500", STRING_1, STRING_2],
    "MAE": [],
    "MSE": [],
    "RMSE": [],
    "MAPE": [],

    "ARC(%)": [],
    "ASD(%)": [],
    "MD": [],
    "IR*(%)": [],
    "IR**(%)": [],
    }

    if isinstance(buy_n_hold, str) or buy_n_hold is None:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)

        df_data["ARC(%)"].append(None)
        df_data["ASD(%)"].append(None)
        df_data["MD"].append(None)
        df_data["IR*(%)"].append(None)
        df_data["IR**(%)"].append(None)
    else:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)

        df_data["ARC(%)"].append(round(ARC(buy_n_hold), 2))
        df_data["ASD(%)"].append(round(ASD(buy_n_hold), 2))
        df_data["MD"].append(round(MaximumDrawdown(buy_n_hold), 2))
        df_data["IR*(%)"].append(round(IR1(buy_n_hold)*100, 2))
        df_data["IR**(%)"].append(round(IR2(buy_n_hold)*100, 2))

    if isinstance(strat_1, str) or strat_1 is None:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)
    
        df_data["ARC(%)"].append(None)
        df_data["ASD(%)"].append(None)
        df_data["MD"].append(None)
        df_data["IR*(%)"].append(None)
        df_data["IR**(%)"].append(None)
    else:
        df_data["MAE"].append(format(mae(buy_n_hold_forecast,strat_1_forecast), ".2e"))
        df_data["MSE"].append(format(mse(buy_n_hold_forecast,strat_1_forecast), ".2e"))
        df_data["RMSE"].append(format(rmse(buy_n_hold_forecast,strat_1_forecast), ".2e"))
        df_data["MAPE"].append(format(mean_absolute_percentage_error(buy_n_hold_forecast,strat_1_forecast), ".2e"))

        df_data["ARC(%)"].append(round(ARC(strat_1), 2))
        df_data["ASD(%)"].append(round(ASD(strat_1), 2))
        df_data["MD"].append(round(MaximumDrawdown(strat_1), 2))
        df_data["IR*(%)"].append(round(IR1(strat_1)*100, 2))
        df_data["IR**(%)"].append(round(IR2(strat_1)*100, 2))

    if isinstance(strat_2, str) or strat_2 is None:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)

        df_data["ARC(%)"].append(None)
        df_data["ASD(%)"].append(None)
        df_data["MD"].append(None)
        df_data["IR*(%)"].append(None)
        df_data["IR**(%)"].append(None)
    else:
        df_data["MAE"].append(format(mae(buy_n_hold_forecast,strat_2_forecast), ".2e"))
        df_data["MSE"].append(format(mse(buy_n_hold_forecast,strat_2_forecast), ".2e"))
        df_data["RMSE"].append(format(rmse(buy_n_hold_forecast,strat_2_forecast), ".2e"))
        df_data["MAPE"].append(format(mean_absolute_percentage_error(buy_n_hold_forecast,strat_2_forecast), ".2e"))

        df_data["ARC(%)"].append(round(ARC(strat_2), 2))
        df_data["ASD(%)"].append(round(ASD(strat_2), 2))
        df_data["MD"].append(round(MaximumDrawdown(strat_2), 2))
        df_data["IR*(%)"].append(round(IR1(strat_2)*100, 2))
        df_data["IR**(%)"].append(round(IR2(strat_2)*100, 2))

    df_perf_metr = pd.DataFrame(data=df_data, index=["","",""])

    return df_perf_metr

def performance_metrics_case_2(STRING_1, STRING_2, STRING_3, df, case):

    buy_n_hold = df[case[-1]]
    buy_n_hold_forecast = df[f'{case[-1]}_forecast']

    strat_1 = df[case[0]]
    strat_1_forecast = df[f'{case[0]}_forecast']

    strat_2 = df[case[1]]
    strat_2_forecast = df[f'{case[1]}_forecast']

    strat_3 = df[case[2]]
    strat_3_forecast = df[f'{case[2]}_forecast']
    
    df_data = {

    "": ["S&P 500", STRING_1, STRING_2, STRING_3],
    "MAE": [],
    "MSE": [],
    "RMSE": [],
    "MAPE": [],

    "ARC(%)": [],
    "ASD(%)": [],
    "MD": [],
    "IR*(%)": [],
    "IR**(%)": [],
    }

    if isinstance(buy_n_hold, str) or buy_n_hold is None:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)

        df_data["ARC(%)"].append(None)
        df_data["ASD(%)"].append(None)
        df_data["MD"].append(None)
        df_data["IR*(%)"].append(None)
        df_data["IR**(%)"].append(None)
    else:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)

        df_data["ARC(%)"].append(round(ARC(buy_n_hold), 2))
        df_data["ASD(%)"].append(round(ASD(buy_n_hold), 2))
        df_data["MD"].append(round(MaximumDrawdown(buy_n_hold), 2))
        df_data["IR*(%)"].append(round(IR1(buy_n_hold)*100, 2))
        df_data["IR**(%)"].append(round(IR2(buy_n_hold)*100, 2))

    if isinstance(strat_1, str) or strat_1 is None:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)
    
        df_data["ARC(%)"].append(None)
        df_data["ASD(%)"].append(None)
        df_data["MD"].append(None)
        df_data["IR*(%)"].append(None)
        df_data["IR**(%)"].append(None)
    else:
        df_data["MAE"].append(format(mae(buy_n_hold_forecast,strat_1_forecast), ".2e"))
        df_data["MSE"].append(format(mse(buy_n_hold_forecast,strat_1_forecast), ".2e"))
        df_data["RMSE"].append(format(rmse(buy_n_hold_forecast,strat_1_forecast), ".2e"))
        df_data["MAPE"].append(format(mean_absolute_percentage_error(buy_n_hold_forecast,strat_1_forecast), ".2e"))

        df_data["ARC(%)"].append(round(ARC(strat_1), 2))
        df_data["ASD(%)"].append(round(ASD(strat_1), 2))
        df_data["MD"].append(round(MaximumDrawdown(strat_1), 2))
        df_data["IR*(%)"].append(round(IR1(strat_1)*100, 2))
        df_data["IR**(%)"].append(round(IR2(strat_1)*100, 2))

    if isinstance(strat_2, str) or strat_2 is None:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)

        df_data["ARC(%)"].append(None)
        df_data["ASD(%)"].append(None)
        df_data["MD"].append(None)
        df_data["IR*(%)"].append(None)
        df_data["IR**(%)"].append(None)
    else:
        df_data["MAE"].append(format(mae(buy_n_hold_forecast,strat_2_forecast), ".2e"))
        df_data["MSE"].append(format(mse(buy_n_hold_forecast,strat_2_forecast), ".2e"))
        df_data["RMSE"].append(format(rmse(buy_n_hold_forecast,strat_2_forecast), ".2e"))
        df_data["MAPE"].append(format(mean_absolute_percentage_error(buy_n_hold_forecast,strat_2_forecast), ".2e"))

        df_data["ARC(%)"].append(round(ARC(strat_2), 2))
        df_data["ASD(%)"].append(round(ASD(strat_2), 2))
        df_data["MD"].append(round(MaximumDrawdown(strat_2), 2))
        df_data["IR*(%)"].append(round(IR1(strat_2)*100, 2))
        df_data["IR**(%)"].append(round(IR2(strat_2)*100, 2))

    if isinstance(strat_3, str) or strat_3 is None:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)

        df_data["ARC(%)"].append(None)
        df_data["ASD(%)"].append(None)
        df_data["MD"].append(None)
        df_data["IR*(%)"].append(None)
        df_data["IR**(%)"].append(None)
    else:
        df_data["MAE"].append(format(mae(buy_n_hold_forecast,strat_3_forecast), ".2e"))
        df_data["MSE"].append(format(mse(buy_n_hold_forecast,strat_3_forecast), ".2e"))
        df_data["RMSE"].append(format(rmse(buy_n_hold_forecast,strat_3_forecast), ".2e"))
        df_data["MAPE"].append(format(mean_absolute_percentage_error(buy_n_hold_forecast,strat_3_forecast), ".2e"))

        df_data["ARC(%)"].append(round(ARC(strat_3), 2))
        df_data["ASD(%)"].append(round(ASD(strat_3), 2))
        df_data["MD"].append(round(MaximumDrawdown(strat_3), 2))
        df_data["IR*(%)"].append(round(IR1(strat_3)*100, 2))
        df_data["IR**(%)"].append(round(IR2(strat_3)*100, 2))

    df_perf_metr = pd.DataFrame(data=df_data, index=["","","",""])

    return df_perf_metr


def performance_metrics_case_3__4(STRING_1, STRING_2, STRING_3, STRING_4, df, case):

    buy_n_hold = df[case[-1]]
    buy_n_hold_forecast = df[f'{case[-1]}_forecast']

    strat_1 = df[case[0]]
    strat_1_forecast = df[f'{case[0]}_forecast']

    strat_2 = df[case[1]]
    strat_2_forecast = df[f'{case[1]}_forecast']

    strat_3 = df[case[2]]
    strat_3_forecast = df[f'{case[2]}_forecast']

    strat_4 = df[case[3]]
    strat_4_forecast = df[f'{case[3]}_forecast']

    
    df_data = {

    "": ["S&P 500", STRING_1, STRING_2, STRING_3, STRING_4],
    "MAE": [],
    "MSE": [],
    "RMSE": [],
    "MAPE": [],

    "ARC(%)": [],
    "ASD(%)": [],
    "MD": [],
    "IR*(%)": [],
    "IR**(%)": [],
    }

    if isinstance(buy_n_hold, str) or buy_n_hold is None:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)

        df_data["ARC(%)"].append(None)
        df_data["ASD(%)"].append(None)
        df_data["MD"].append(None)
        df_data["IR*(%)"].append(None)
        df_data["IR**(%)"].append(None)
    else:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)

        df_data["ARC(%)"].append(round(ARC(buy_n_hold), 2))
        df_data["ASD(%)"].append(round(ASD(buy_n_hold), 2))
        df_data["MD"].append(round(MaximumDrawdown(buy_n_hold), 2))
        df_data["IR*(%)"].append(round(IR1(buy_n_hold)*100, 2))
        df_data["IR**(%)"].append(round(IR2(buy_n_hold)*100, 2))

    if isinstance(strat_1, str) or strat_1 is None:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)
    
        df_data["ARC(%)"].append(None)
        df_data["ASD(%)"].append(None)
        df_data["MD"].append(None)
        df_data["IR*(%)"].append(None)
        df_data["IR**(%)"].append(None)
    else:
        df_data["MAE"].append(format(mae(buy_n_hold_forecast,strat_1_forecast), ".2e"))
        df_data["MSE"].append(format(mse(buy_n_hold_forecast,strat_1_forecast), ".2e"))
        df_data["RMSE"].append(format(rmse(buy_n_hold_forecast,strat_1_forecast), ".2e"))
        df_data["MAPE"].append(format(mean_absolute_percentage_error(buy_n_hold_forecast,strat_1_forecast), ".2e"))

        df_data["ARC(%)"].append(round(ARC(strat_1), 2))
        df_data["ASD(%)"].append(round(ASD(strat_1), 2))
        df_data["MD"].append(round(MaximumDrawdown(strat_1), 2))
        df_data["IR*(%)"].append(round(IR1(strat_1)*100, 2))
        df_data["IR**(%)"].append(round(IR2(strat_1)*100, 2))

    if isinstance(strat_2, str) or strat_2 is None:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)

        df_data["ARC(%)"].append(None)
        df_data["ASD(%)"].append(None)
        df_data["MD"].append(None)
        df_data["IR*(%)"].append(None)
        df_data["IR**(%)"].append(None)
    else:
        df_data["MAE"].append(format(mae(buy_n_hold_forecast,strat_2_forecast), ".2e"))
        df_data["MSE"].append(format(mse(buy_n_hold_forecast,strat_2_forecast), ".2e"))
        df_data["RMSE"].append(format(rmse(buy_n_hold_forecast,strat_2_forecast), ".2e"))
        df_data["MAPE"].append(format(mean_absolute_percentage_error(buy_n_hold_forecast,strat_2_forecast), ".2e"))

        df_data["ARC(%)"].append(round(ARC(strat_2), 2))
        df_data["ASD(%)"].append(round(ASD(strat_2), 2))
        df_data["MD"].append(round(MaximumDrawdown(strat_2), 2))
        df_data["IR*(%)"].append(round(IR1(strat_2)*100, 2))
        df_data["IR**(%)"].append(round(IR2(strat_2)*100, 2))

    if isinstance(strat_3, str) or strat_3 is None:
        df_data["MAE"].append(None)
        df_data["MSE"].append(None)
        df_data["RMSE"].append(None)
        df_data["MAPE"].append(None)

        df_data["ARC(%)"].append(None)
        df_data["ASD(%)"].append(None)
        df_data["MD"].append(None)
        df_data["IR*(%)"].append(None)
        df_data["IR**(%)"].append(None)
    else:
        df_data["MAE"].append(format(mae(buy_n_hold_forecast,strat_3_forecast), ".2e"))
        df_data["MSE"].append(format(mse(buy_n_hold_forecast,strat_3_forecast), ".2e"))
        df_data["RMSE"].append(format(rmse(buy_n_hold_forecast,strat_3_forecast), ".2e"))
        df_data["MAPE"].append(format(mean_absolute_percentage_error(buy_n_hold_forecast,strat_3_forecast), ".2e"))

        df_data["ARC(%)"].append(round(ARC(strat_3), 2))
        df_data["ASD(%)"].append(round(ASD(strat_3), 2))
        df_data["MD"].append(round(MaximumDrawdown(strat_3), 2))
        df_data["IR*(%)"].append(round(IR1(strat_3)*100, 2))
        df_data["IR**(%)"].append(round(IR2(strat_3)*100, 2))

    if isinstance(strat_4, str) or strat_4 is None:
            df_data["MAE"].append(None)
            df_data["MSE"].append(None)
            df_data["RMSE"].append(None)
            df_data["MAPE"].append(None)

            df_data["ARC(%)"].append(None)
            df_data["ASD(%)"].append(None)
            df_data["MD"].append(None)
            df_data["IR*(%)"].append(None)
            df_data["IR**(%)"].append(None)
    else:
        df_data["MAE"].append(format(mae(buy_n_hold_forecast,strat_4_forecast), ".2e"))
        df_data["MSE"].append(format(mse(buy_n_hold_forecast,strat_4_forecast), ".2e"))
        df_data["RMSE"].append(format(rmse(buy_n_hold_forecast,strat_4_forecast), ".2e"))
        df_data["MAPE"].append(format(mean_absolute_percentage_error(buy_n_hold_forecast,strat_4_forecast), ".2e"))

        df_data["ARC(%)"].append(round(ARC(strat_4), 2))
        df_data["ASD(%)"].append(round(ASD(strat_4), 2))
        df_data["MD"].append(round(MaximumDrawdown(strat_4), 2))
        df_data["IR*(%)"].append(round(IR1(strat_4)*100, 2))
        df_data["IR**(%)"].append(round(IR2(strat_4)*100, 2))

    df_perf_metr = pd.DataFrame(data=df_data, index=["","","","",""])

    return df_perf_metr

