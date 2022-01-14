import numpy

# Function that partition a list in chunks
def partition(l, n):
    for i in range(0, len(l), n):
        yield l[i:i + n]

# Swap function
def listSwap ( n1, n2, l):
    tmp = l.copy()
    tmp[n1],tmp[n2] = tmp[n2],tmp[n1]
    return(tmp)
        
# MTD wrapping
def MTD(l):
    return "MTD["+str(l[0])+","+str(l[1])+"]"

def MTDList(l):
    return( list( map( MTD, list(partition(l, 2))) ) )
        
# Read the tensor valence
maxTensorValence = int(input("Set the tensor valence: "))

for tensorValence in range( 1, maxTensorValence + 1):

    # Make a lsit [m_1, n_1, ... m_k, n_k]
    indexArray = list( map( lambda x: [ "m" + str(x) , "n" + str(x) ] , list( range( 1 , tensorValence+1 ) ) ) )
    indexArray = list( numpy.concatenate(indexArray).flat )
    
    # Make a list of transmutations
    transmutationsArray = list( [ indexArray.copy() ] )

    # Generate an array of all pair indices permutations
    for i in range(tensorValence):
        L = len(transmutationsArray)
        for j in range(L):
            transmutationsArray.append( listSwap(2*i, 2*i+1, transmutationsArray[j] ) )
            
    # Shift everything left
    transmutationsArray = list(map(lambda x: x[1:] + x[:1] , transmutationsArray))
    
    # Convert the transmutation array to MTD
    transmutationsArray = list( map( MTDList,  transmutationsArray) )
    transmutationsArray = list( map( lambda x: "".join(x) ,transmutationsArray) )
    
    # Export to a file
    f = open("ITensor_"+str(tensorValence),"w")
    f.write( "(1/2)^("+str(tensorValence)+")  ("+("+".join(transmutationsArray))+")" )
    f.close()

            
