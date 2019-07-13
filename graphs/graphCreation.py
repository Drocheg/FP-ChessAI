import numpy as np
import matplotlib.pyplot as plt

def printPlayerDate(color, variable_name, player_name, data):
    print(player_name)
    print(np.mean(data))

    plt.plot(data, 'o', label=player_name)
    plt.xlabel('Turns')
    plt.ylabel(variable_name)
    plt.title(color)
    plt.tight_layout()
    plt.legend(loc=2, prop={'size': 15})
    plt.tick_params(axis='both', which='major', labelsize=15)
    plt.tick_params(axis='both', which='minor', labelsize=15)



def printInformation(data, variable_name):
    fig1 = plt.figure(figsize=(10, 4), dpi=120)
    data1 = data[::2]
    data2 = data[1::2]
    printPlayerDate("plot", variable_name, "Minimax Alpha-Beta Sorted", data1)
    printPlayerDate("plot", variable_name, "Minimax Alpha-Beta", data2)
    plt.show()

#times = [1562987115715,1562987115614,1562987114409,1562987114214,1562987113074,1562987108310,1562987106376,1562987102389,1562987102193,1562987099449,1562987097710,1562987095563,1562987093717,1562987091859,1562987090482,1562987088284,1562987087263,1562987086964,1562987085767,1562987083794,1562987082544,1562987080820,1562987080498,1562987078638,1562987077730,1562987077617,1562987077194,1562987076854,1562987075753,1562987074110,1562987072683,1562987071391,1562987070351,1562987068624,1562987066198,1562987065396,1562987065326,1562987065068,1562987064252,1562987063673,1562987063081,1562987062528,1562987062093,1562987061138,1562987059413,1562987058568,1562987058358,1562987058223,1562987056475,1562987055970,1562987055502,1562987054089,1562987053382,1562987052305,1562987051893,1562987048818,1562987047781,1562987045748,1562987044629,1562987043914,1562987043464,1562987042751,1562987042497,1562987042179,1562987042016]
times     = [1562992236480,1562992236371,1562992235107,1562992234896,1562992233767,1562992228909,1562992226941,1562992222988,1562992222799,1562992220018,1562992218372,1562992216392,1562992214617,1562992212525,1562992211090,1562992208872,1562992207871,1562992207567,1562992206457,1562992204395,1562992203138,1562992201233,1562992200530,1562992198499,1562992197570,1562992197448,1562992197001,1562992196625,1562992195580,1562992193942,1562992192543,1562992191271,1562992190205,1562992188520,1562992186110,1562992185330,1562992185269,1562992185005,1562992184192,1562992183650,1562992182908,1562992182317,1562992181853,1562992180924,1562992179111,1562992178287,1562992178075,1562992177940,1562992176046,1562992175509,1562992175016,1562992173572,1562992172862,1562992171759,1562992171321,1562992168588,1562992167254,1562992165143,1562992164032,1562992163313,1562992162844,1562992162047,1562992161784,1562992161467,1562992161278]

#ndxE = [9748,8785,20395,6054,419894,15991,379492,2349,286496,11311,208716,14576,195227,12171,216819,10756,29693,10841,199747,10629,165128,3408,145187,8674,9878,2835,32130,7719,142319,12956,113187,11948,144599,25304,63780,1538,22539,18515,45924,15495,53861,6041,82697,26222,76334,2677,10499,19024,46154,5279,124032,10455,94316,3107,219557,14239,169659,12802,61209,6726,58089,2983,26267,2004,0]
ndxE    = [9748,8785,20395,6054,419894,15991,379492,2349,286496,11311,208716,14576,195227,12171,216819,10756,29693,10841,199747,10629,165128,3408,145187,8674,9878,2835,32130,7719,142319,12956,113187,11948,144599,25304,63780,1538,22539,18515,45924,15495,53861,6041,82697,26222,76334,2677,10499,19024,46154,5279,124032,10455,94316,3107,219557,14239,169659,12802,61209,6726,58089,2983,26267,2004,0]
timesDifferences = [times[i] - times[i+1] for i in range(len(times)-1)]
ndxE = ndxE[:-1]

timesDifferences.reverse()
ndxE.reverse()


printInformation(timesDifferences, 'Time (ms)')
printInformation(ndxE, 'Boards explored')
