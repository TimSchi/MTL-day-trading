## Data download and transformation

## Import required packages
import numpy
import talib as ta
import pandas
import itertools
import pandas_datareader.data as web
import datetime as dt

## Define sample stocks of indexes by symbols 
#FTSE100: London
ftse = ["CPI","REX","BG","AGK","BA","AZN","AAL","AMFW","CNA","LGEN","BARC","BP","REL","STAN","GSK","PRU","LAND","MGGT","RR","DGE","SAB","NG","BLND","RIO","WTB","SMIN","RB","RDSB","LLOY","SGE","SHP","NXT","IMI","BLT","MKS","MRW","RBS","WEIR","WOS","HMSO","SRP","IMB","GKN","ADN","SVT","SKY","VOD","TSCO","SDR","UU","GFS","HSBA","RSA","OML","TATE","SN","AV","KGF","PSON","BATS","ULVR"]
#DAX30: Frankfurt Xetra
dax  = ["ALV","BAS","BAYN","BMW","CBK","DBK","LHA","HEN","LIN","RWE","SIE","VOW","SAP","MUV2","DTE","ADS","DAI","TKA","FME","EOAN"]
#DJIA: NYSE and NASDAQ
djia = ["GE","XOM","PG","DD","MMM","UTX","IBM","MRK","AXP","MCD","BA","KO","CAT","DIS","JPM","JNJ","WMT","HD","INTC","MSFT"]
#CAC40: Paris
cac  = ["AC","AI","CA","OR","MC","ML","SGO","GLE","CS","BNP","EN","BN","ENGI","RI","UG","SU","FP","VIE","VIV","CAP","AIR","KER","ORA","ACA"]

#Investment period
start = dt.datetime(2000, 4, 1)
end   = dt.datetime(2013, 4, 30)

## Define baseline reset function
def reset():
    a = []  #buy and hold
    b = []  #short and hold
    c = []   #random
    return(a,b,c)
## Define normalize function
def nor(b):
    mi = numpy.nanmin(b)
    ma = numpy.nanmax(b)
    if (ma - mi==0): return 0
    else: return (b - mi) / (ma - mi)
## Define technical analysis application function
def TA(S,s,c):        
    S      = S.fillna(0)    
    ope    = numpy.asfarray(S.Open)
    high   = numpy.asfarray(S.High)
    low    = numpy.asfarray(S.Low)
    close  = numpy.asfarray(S.Close)                
    volume = numpy.asfarray(S.Volume)
                             
    ##ROI calculation
    ROI = [(close[i+1]-close[i])/close[i] for i in range(len(close)-1)]
    ROI.append(0) #add zero value for last day
    d   = pandas.DataFrame(ROI,index=S.index,columns=['ROI'])
    d.to_csv("C:\\Users\\...\\Documents\\Data_TA\\{0}\\ROI_{1}.csv" .format(s,c))

    ##Baselines
    try:
        bah.append((S.Close['2013-04-30']-S.Close['2002-05-01'])/S.Close['2002-05-01'])
        sah.append((-S.Close['2013-04-30']+S.Close['2002-05-01'])/S.Close['2002-05-01'])
    except:
        bah.append((S.Close['2013-04-30']-S.Close['2002-05-02'])/S.Close['2002-05-02'])
        sah.append((-S.Close['2013-04-30']+S.Close['2002-05-02'])/S.Close['2002-05-02'])
        
    rp.append(numpy.dot(numpy.random.uniform(-1,1,len(S)),numpy.asfarray(ROI)))
    
    ##talib application
    #overlap
    BBANDS = ta.BBANDS(close)
    DEMA = ta.DEMA(close)
    EMA = ta.EMA(close)
    HT_TRENDLINE = ta.HT_TRENDLINE(close)
    KAMA = ta.KAMA(close)
    MA = ta.MA(close)
    MAMA = ta.MAMA(close)
    MIDPOINT = ta.MIDPOINT(close)
    MIDPRICE = ta.MIDPRICE(high,low)
    SAR = ta.SAR(high,low)
    SAREXT = ta.SAREXT(high,low)
    SMA = ta.SMA(close)
    T3 = ta.T3(close)
    TEMA = ta.TEMA(close)
    TRIMA = ta.TRIMA(close)
    WMA = ta.WMA(close)
    #momentum
    ADX  = ta.ADX(high,low,close)
    ADXR = ta.ADXR(high,low,close)
    APO  = ta.APO(close)
    AROON = ta.AROON(high,low)
    AROONOSC = ta.AROONOSC(high,low)
    BOP = ta.BOP(ope,high,low,close)
    CCI = ta.CCI(high,low,close)
    CMO = ta.CMO(close)
    DX  = ta.DX(high,low,close)
    MACD = ta.MACD(close)
    MACDEXT = ta.MACDEXT(close)
    MFI = ta.MFI(high,low,close,volume)
    MINUS_DI = ta.MINUS_DI(high,low,close)
    MINUS_DM = ta.MINUS_DM(high,low)
    MOM = ta.MOM(close)
    PLUS_DI = ta.PLUS_DI(high,low,close)
    PLUS_DM = ta.PLUS_DM(high,low)
    PPO = ta.PPO(close)
    ROC = ta.ROC(close)
    ROCP = ta.ROCP(close)
    ROCR = ta.ROCR(close)
    RSI  = ta.RSI(close)
    STOCH  = ta.STOCH(high,low,close)
    STOCHF = ta.STOCHF(high,low,close)
    STOCHRSI = ta.STOCHRSI(close)
    TRIX = ta.TRIX(close)
    ULTOSC = ta.ULTOSC(high,low,close)
    WILLR  = ta.WILLR(high,low,close)
    #volume
    AD = ta.AD(high,low,close,volume)
    ADOSC = ta.ADOSC(high,low,close,volume)
    OBV = ta.OBV(close,volume)
    #cycle
    HT_DCPERIOD = ta.HT_DCPERIOD(close)
    HT_DCPHASE  = ta.HT_DCPHASE(close)
    HT_PHASOR = ta.HT_PHASOR(close)
    HT_SINE = ta.HT_SINE(close)
    HT_TRENDMODE = ta.HT_TRENDMODE(close)
    #price
    AVGPRICE = ta.AVGPRICE(ope,high,low,close)
    MEDPRICE = ta.MEDPRICE(high,low)
    TYPPRICE = ta.TYPPRICE(high,low,close)
    WCLPRICE = ta.WCLPRICE(high,low,close)
    #volatility
    ATR = ta.ATR(high,low,close)
    NATR = ta.NATR(high,low,close)
    TRANGE = ta.TRANGE(high,low,close)
    #pattern
    CDL2CROWS = ta.CDL2CROWS(ope,high,low,close)
    CDL3BLACKCROWS = ta.CDL3BLACKCROWS(ope,high,low,close)
    CDL3INSIDE = ta.CDL3INSIDE(ope,high,low,close)
    CDL3LINESTRIKE = ta.CDL3LINESTRIKE(ope,high,low,close)
    CDL3OUTSIDE = ta.CDL3OUTSIDE(ope,high,low,close)
    CDL3STARSINSOUTH  = ta.CDL3STARSINSOUTH(ope,high,low,close)
    CDL3WHITESOLDIERS = ta.CDL3WHITESOLDIERS(ope,high,low,close)
    CDLABANDONEDBABY  = ta.CDLABANDONEDBABY(ope,high,low,close)
    CDLADVANCEBLOCK = ta.CDLADVANCEBLOCK(ope,high,low,close)
    CDLBELTHOLD  = ta.CDLBELTHOLD(ope,high,low,close)
    CDLBREAKAWAY = ta.CDLBREAKAWAY(ope,high,low,close)
    CDLCLOSINGMARUBOZU  = ta.CDLCLOSINGMARUBOZU(ope,high,low,close)
    CDLCONCEALBABYSWALL = ta.CDLCONCEALBABYSWALL(ope,high,low,close)
    CDLCOUNTERATTACK  = ta.CDLCOUNTERATTACK(ope,high,low,close)
    CDLDARKCLOUDCOVER = ta.CDLDARKCLOUDCOVER(ope,high,low,close)
    CDLDOJI = ta.CDLDOJI(ope,high,low,close)
    CDLDOJISTAR = ta.CDLDOJISTAR(ope,high,low,close)
    CDLDRAGONFLYDOJI = ta.CDLDRAGONFLYDOJI(ope,high,low,close)
    CDLENGULFING = ta.CDLENGULFING(ope,high,low,close)
    CDLEVENINGDOJISTAR = ta.CDLEVENINGDOJISTAR(ope,high,low,close)
    CDLEVENINGSTAR = ta.CDLEVENINGSTAR(ope,high,low,close)
    CDLGAPSIDESIDEWHITE = ta.CDLGAPSIDESIDEWHITE(ope,high,low,close)
    CDLGRAVESTONEDOJI = ta.CDLGRAVESTONEDOJI(ope,high,low,close)
    CDLHAMMER = ta.CDLHAMMER(ope,high,low,close)
    CDLHANGINGMAN = ta.CDLHANGINGMAN(ope,high,low,close)
    CDLHARAMI = ta.CDLHARAMI(ope,high,low,close)
    CDLHARAMICROSS = ta.CDLHARAMICROSS(ope,high,low,close)
    CDLHIGHWAVE = ta.CDLHIGHWAVE(ope,high,low,close)
    CDLHIKKAKE  = ta.CDLHIKKAKE(ope,high,low,close)
    CDLHIKKAKEMOD = ta.CDLHIKKAKEMOD(ope,high,low,close)
    CDLHOMINGPIGEON = ta.CDLHOMINGPIGEON(ope,high,low,close)
    CDLIDENTICAL3CROWS = ta.CDLIDENTICAL3CROWS(ope,high,low,close)
    CDLINNECK = ta.CDLINNECK(ope,high,low,close)
    CDLINVERTEDHAMMER = ta.CDLINVERTEDHAMMER(ope,high,low,close)
    CDLKICKING = ta.CDLKICKING(ope,high,low,close)
    CDLKICKINGBYLENGTH = ta.CDLKICKINGBYLENGTH(ope,high,low,close)
    CDLLADDERBOTTOM = ta.CDLLADDERBOTTOM(ope,high,low,close)
    CDLLONGLEGGEDDOJI = ta.CDLLONGLEGGEDDOJI(ope,high,low,close)
    CDLLONGLINE = ta.CDLLONGLINE(ope,high,low,close)
    CDLMARUBOZU = ta.CDLMARUBOZU(ope,high,low,close)
    CDLMATCHINGLOW = ta.CDLMATCHINGLOW(ope,high,low,close)
    CDLMATHOLD = ta.CDLMATHOLD(ope,high,low,close)
    CDLMORNINGDOJISTAR = ta.CDLMORNINGDOJISTAR(ope,high,low,close)
    CDLMORNINGSTAR = ta.CDLMORNINGSTAR(ope,high,low,close)
    CDLONNECK = ta.CDLONNECK(ope,high,low,close)
    CDLPIERCING = ta.CDLPIERCING(ope,high,low,close)
    CDLRICKSHAWMAN = ta.CDLRICKSHAWMAN(ope,high,low,close)
    CDLRISEFALL3METHODS = ta.CDLRISEFALL3METHODS(ope,high,low,close)
    CDLSEPARATINGLINES  = ta.CDLSEPARATINGLINES(ope,high,low,close)
    CDLSHOOTINGSTAR = ta.CDLSHOOTINGSTAR(ope,high,low,close)
    CDLSHORTLINE = ta.CDLSHORTLINE(ope,high,low,close)
    CDLSPINNINGTOP = ta.CDLSPINNINGTOP(ope,high,low,close)
    CDLSTALLEDPATTERN = ta.CDLSTALLEDPATTERN(ope,high,low,close)
    CDLSTICKSANDWICH  = ta.CDLSTICKSANDWICH(ope,high,low,close)
    CDLTAKURI = ta.CDLTAKURI(ope,high,low,close)
    CDLTASUKIGAP = ta.CDLTASUKIGAP(ope,high,low,close)
    CDLTHRUSTING = ta.CDLTHRUSTING(ope,high,low,close)
    CDLTRISTAR = ta.CDLTRISTAR(ope,high,low,close)
    CDLUNIQUE3RIVER = ta.CDLUNIQUE3RIVER(ope,high,low,close)
    CDLUPSIDEGAP2CROWS  = ta.CDLUPSIDEGAP2CROWS(ope,high,low,close)
    CDLXSIDEGAP3METHODS = ta.CDLXSIDEGAP3METHODS(ope,high,low,close)
        
    f  = numpy.column_stack((ATR, NATR, TRANGE, HT_DCPERIOD, HT_DCPHASE, HT_PHASOR[0],HT_PHASOR[1], HT_SINE[0],HT_SINE[1], HT_TRENDMODE, AVGPRICE, MEDPRICE, TYPPRICE, WCLPRICE, ADX, ADXR, APO, AROON[0], AROON[1],AROONOSC, BOP, CCI, CMO, DX, MACD[0], MACD[1],MACD[2],MACDEXT[0],MACDEXT[1],MACDEXT[2], MFI, MINUS_DI, MINUS_DM, MOM, PLUS_DI, PLUS_DM, PPO, ROC, ROCP, ROCR, RSI, STOCH[0],STOCH[1], STOCHF[0],STOCHF[1], STOCHRSI[0],STOCHRSI[1], TRIX, ULTOSC, WILLR, CDL2CROWS, CDL3BLACKCROWS, CDL3INSIDE, CDL3LINESTRIKE, CDL3OUTSIDE, CDL3STARSINSOUTH, CDL3WHITESOLDIERS, CDLABANDONEDBABY, CDLADVANCEBLOCK, CDLBELTHOLD, CDLBREAKAWAY, CDLCLOSINGMARUBOZU, CDLCONCEALBABYSWALL, CDLCOUNTERATTACK, CDLDARKCLOUDCOVER, CDLDOJI, CDLDOJISTAR, CDLDRAGONFLYDOJI, CDLENGULFING, CDLEVENINGDOJISTAR, CDLEVENINGSTAR, CDLGAPSIDESIDEWHITE, CDLGRAVESTONEDOJI, CDLHAMMER, CDLHANGINGMAN, CDLHARAMI, CDLHARAMICROSS, CDLHIGHWAVE, CDLHIKKAKE, CDLHIKKAKEMOD, CDLHOMINGPIGEON, CDLIDENTICAL3CROWS, CDLINNECK, CDLINVERTEDHAMMER, CDLKICKING, CDLKICKINGBYLENGTH, CDLLADDERBOTTOM, CDLLONGLEGGEDDOJI, CDLLONGLINE, CDLMARUBOZU, CDLMATCHINGLOW, CDLMATHOLD, CDLMORNINGDOJISTAR, CDLMORNINGSTAR, CDLONNECK, CDLPIERCING, CDLRICKSHAWMAN, CDLRISEFALL3METHODS, CDLSEPARATINGLINES, CDLSHOOTINGSTAR, CDLSHORTLINE, CDLSPINNINGTOP, CDLSTALLEDPATTERN, CDLSTICKSANDWICH, CDLTAKURI, CDLTASUKIGAP, CDLTHRUSTING, CDLTRISTAR, CDLUNIQUE3RIVER, CDLUPSIDEGAP2CROWS, CDLXSIDEGAP3METHODS, BBANDS[0], BBANDS[1],BBANDS[2],DEMA, EMA, HT_TRENDLINE, KAMA, MA, MAMA[0],MAMA[1], MIDPOINT, MIDPRICE, SAR, SAREXT, SMA, T3, TEMA, TRIMA, WMA, AD, ADOSC, OBV))
    
    h  = numpy.apply_along_axis(nor, 0, f) # normalize columnwise
    
    df = pandas.DataFrame(h,index=S.index,columns=['ATR','NATR', 'TRANGE', 'HT_DCPERIOD', 'HT_DCPHASE', 'HT_PHASOR[0]','HT_PHASOR[1]', 'HT_SINE[0]','HT_SINE[1]', 'HT_TRENDMODE', 'AVGPRICE', 'MEDPRICE', 'TYPPRICE', 'WCLPRICE', 'ADX', 'ADXR', 'APO', 'AROON[0]', 'AROON[1]','AROONOSC', 'BOP', 'CCI', 'CMO', 'DX', 'MACD[0]', 'MACD[1]','MACD[2]','MACDEXT[0]','MACDEXT[1]','MACDEXT[2]', 'MFI', 'MINUS_DI', 'MINUS_DM', 'MOM', 'PLUS_DI', 'PLUS_DM', 'PPO', 'ROC', 'ROCP', 'ROCR', 'RSI', 'STOCH[0]','STOCH[1]', 'STOCHF[0]','STOCHF[1]', 'STOCHRSI[0]','STOCHRSI[1]', 'TRIX', 'ULTOSC', 'WILLR', 'CDL2CROWS', 'CDL3BLACKCROWS', 'CDL3INSIDE', 'CDL3LINESTRIKE', 'CDL3OUTSIDE', 'CDL3STARSINSOUTH', 'CDL3WHITESOLDIERS', 'CDLABANDONEDBABY', 'CDLADVANCEBLOCK', 'CDLBELTHOLD', 'CDLBREAKAWAY', 'CDLCLOSINGMARUBOZU', 'CDLCONCEALBABYSWALL', 'CDLCOUNTERATTACK', 'CDLDARKCLOUDCOVER', 'CDLDOJI', 'CDLDOJISTAR', 'CDLDRAGONFLYDOJI', 'CDLENGULFING', 'CDLEVENINGDOJISTAR', 'CDLEVENINGSTAR', 'CDLGAPSIDESIDEWHITE', 'CDLGRAVESTONEDOJI', 'CDLHAMMER', 'CDLHANGINGMAN', 'CDLHARAMI', 'CDLHARAMICROSS', 'CDLHIGHWAVE', 'CDLHIKKAKE', 'CDLHIKKAKEMOD', 'CDLHOMINGPIGEON', 'CDLIDENTICAL3CROWS', 'CDLINNECK', 'CDLINVERTEDHAMMER', 'CDLKICKING', 'CDLKICKINGBYLENGTH', 'CDLLADDERBOTTOM', 'CDLLONGLEGGEDDOJI', 'CDLLONGLINE', 'CDLMARUBOZU', 'CDLMATCHINGLOW', 'CDLMATHOLD', 'CDLMORNINGDOJISTAR', 'CDLMORNINGSTAR', 'CDLONNECK', 'CDLPIERCING', 'CDLRICKSHAWMAN', 'CDLRISEFALL3METHODS', 'CDLSEPARATINGLINES', 'CDLSHOOTINGSTAR', 'CDLSHORTLINE', 'CDLSPINNINGTOP', 'CDLSTALLEDPATTERN', 'CDLSTICKSANDWICH', 'CDLTAKURI', 'CDLTASUKIGAP', 'CDLTHRUSTING', 'CDLTRISTAR', 'CDLUNIQUE3RIVER', 'CDLUPSIDEGAP2CROWS', 'CDLXSIDEGAP3METHODS', 'BBANDS[0]', 'BBANDS[1]','BBANDS[2]','DEMA', 'EMA', 'HT_TRENDLINE', 'KAMA', 'MA', 'MAMA[0]','MAMA[1]', 'MIDPOINT', 'MIDPRICE', 'SAR', 'SAREXT', 'SMA', 'T3', 'TEMA', 'TRIMA', 'WMA', 'AD', 'ADOSC', 'OBV'])
    df.to_csv("C:\\Users\\...\\Documents\\Data_TA\\{0}\\{1}.csv" .format(s,c))

## Simulation function of buy-and-hold baseline    
def sim(bah):
    TC = 0.006
    startbudget = 0.5
    v = (startbudget/(1+TC) + startbudget) * (1 + numpy.asfarray(bah))
    endbalance_sim_bah = sum(v)
    return(endbalance_sim_bah)

## Download and save stock data for each index, compute baselines
bah,sah,rp=reset()
s = 'CAC'
for c in cac:
    S = web.DataReader("%s.PA" %c, 'yahoo', start, end)    
    TA(S,s,c)

v = sim(bah)    
print('{0}\n profit_bah:{1}\n profit_sah:{2}\n profit_rp:{3}\n endbalance_sim_bah:{4}\n'.format(s,sum(bah),sum(sah),sum(rp),v))

bah,sah,rp=reset()
s = 'DAX'
for c in dax:
    S = web.DataReader("%s.DE" %c, 'yahoo', start, end)    
    TA(S,s,c)

v = sim(bah)    
print('{0}\n profit_bah:{1}\n profit_sah:{2}\n profit_rp:{3}\n endbalance_sim_bah:{4}\n'.format(s,sum(bah),sum(sah),sum(rp),v))
    
bah,sah,rp=reset()
s = 'DJIA'
for c in djia:
    if c in {"INTC","MSFT"}:
        S = web.DataReader("NASDAQ:%s" %c, 'google', start, end)
    else:
        S = web.DataReader("NYSE:%s" %c, 'google', start, end)    
    TA(S,s,c)

v = sim(bah)    
print('{0}\n profit_bah:{1}\n profit_sah:{2}\n profit_rp:{3}\n endbalance_sim_bah:{4}\n'.format(s,sum(bah),sum(sah),sum(rp),v))
    
bah,sah,rp=reset()
s = 'FTSE'
for c in ftse:   
    S = web.DataReader("%s.L" %c, 'google', start, end)    
    TA(S,s,c)

v = sim(bah)    
print('{0}\n profit_bah:{1}\n profit_sah:{2}\n profit_rp:{3}\n endbalance_sim_bah:{4}\n'.format(s,sum(bah),sum(sah),sum(rp),v))


