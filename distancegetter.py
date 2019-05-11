import requests
import csv

destination = "Los Angeles"
towns = set()

with open("MembershipRolls.csv", "rU") as rolls:
    r = csv.reader(rolls)
    for row in r:
        towns.add(row[5])

print(len(towns), "uniqe towns")

# Get distance info
hometown_dict = {}
##for hometown in towns:
##    response = requests.get("https://www.distance24.org/route.json?stops="+hometown+"|"+destination)
##    data = response.json()
##    dist = data["distance"]
##    if dist == 0:
##        print("Invalid town: ", hometown)
##    hometown_dict[hometown] = dist

for hometown in towns:
    response = requests.get("https://maps.googleapis.com/maps/api/distancematrix/json?units=imperial&origins=LosAngeles,CA&destinations="+hometown+"&key=AIzaSyDHZoOke3Lz4D0KFqjdynNuRi5UrfjXOgg")
    data = response.json()

    # Distance in meters
    try:
        dist = data["rows"][0]["elements"][0]["distance"]["value"]
        if dist == 0:
            print("Invalid town: ", hometown)
        hometown_dict[hometown] = dist
    except:
        print("OOF")
        print(hometown)
        hometown_dict[hometown] = -1
        
    


# Databases for exchange

state_pops = {"New York": 13479142, 
"Pennsylvania": 9900180, 
"Illinois": 7897241, 
"Ohio": 6907612, 
"California": 6907387, 
"Texas": 6414824, 
"Michigan": 5256106, 
"Massachusetts": 4316721, 
"New Jersey": 4160165, 
"Missouri": 3784664, 
"North Carolina": 3571623, 
"Indiana": 3427796, 
"Wisconsin": 3137587, 
"Georgia": 3123723, 
"Tennessee": 2915841, 
"Kentucky": 2845627, 
"Alabama": 2832961, 
"Minnesota": 2792300, 
"Virginia": 2677773, 
"Iowa": 2538268, 
"Louisiana": 2363880, 
"Oklahoma": 2336434, 
"Mississippi": 2183796, 
"West Virginia": 1961974, 
"Arkansas": 1949387, 
"South Carolina": 1899804, 
"Florida": 1897414, 
"Maryland": 1821244, 
"Kansas": 1801028, 
"Washington": 1736191, 
"Connecticut": 1709242, 
"Nebraska": 1315834, 
"Colorado": 1123296, 
"Oregon": 1089684, 
"Maine": 847226, 
"Rhode Island": 713346, 
"District of Columbia": 663091, 
"South Dakota": 642961, 
"North Dakota": 641935, 
"Montana": 559456, 
"Utah": 550310, 
"New Mexico": 531818, 
"Idaho": 524873, 
"Arizona": 499261, 
"New Hampshire": 491524, 
"Hawaii": 423330, 
"Vermont": 359231, 
"Delaware": 266505, 
"Wyoming": 250742, 
"Nevada": 110247, 
"Alaska": 72524}

nicknames = {"NY": "New York", 
"PA": "Pennsylvania", 
"IL": "Illinois", 
"OH": "Ohio", 
"CA": "California", 
"TX": "Texas", 
"MI": "Michigan", 
"MA": "Massachusetts", 
"NJ": "New Jersey", 
"MO": "Missouri", 
"NC": "North Carolina", 
"IN": "Indiana", 
"WI": "Wisconsin", 
"GA": "Georgia", 
"TN": "Tennessee", 
"KY": "Kentucky", 
"AL": "Alabama", 
"MN": "Minnesota", 
"VA": "Virginia", 
"IA": "Iowa", 
"LA": "Louisiana", 
"OK": "Oklahoma", 
"MS": "Mississippi", 
"WV": "West Virginia", 
"AR": "Arkansas", 
"SC": "South Carolina", 
"FL": "Florida", 
"MD": "Maryland", 
"KS": "Kansas", 
"WA": "Washington", 
"CT": "Connecticut", 
"NE": "Nebraska", 
"CO": "Colorado", 
"OR": "Oregon", 
"ME": "Maine", 
"RI": "Rhode Island", 
"DC": "District of Columbia", 
"SD": "South Dakota", 
"ND": "North Dakota", 
"MT": "Montana", 
"UT": "Utah", 
"NM": "New Mexico", 
"ID": "Idaho", 
"AZ": "Arizona", 
"NH": "New Hampshire", 
"HI": "Hawaii", 
"VT": "Vermont", 
"DE": "Delaware", 
"WY": "Wyoming", 
"NV": "Nevada", 
"AK": "Alaska"}

def get_state(town_str):
    if "," in town_str:
        state = town_str[town_str.index(",")+1:].strip()
        if state in nicknames:
            return nicknames[state]
        else:
            print("OOPS:", state)
            return state
    else:
        print("Bare", town_str)
        return town_str

    


with open("MembershipRolls.csv", "r") as rolls:
    with open("MembershipWithDistance.csv", "w") as output:
        r = csv.reader(rolls)
        wr = csv.writer(output)
        
        all = []
        header = r.next()
        header.append("Distance")
        header.append("State Population")
        all.append(header)
        print(header)
        for row in r:
            town_name = row[5]

            # Add distance
            row.append(hometown_dict[town_name])
            #row.append("a distance")

            # Add state pop
            state = get_state(town_name)
            if state in state_pops:
                pop = state_pops[state]
            else:
                print("Fuck", state)
                pop = -1
            row.append(pop)

            # Put the new row in the list for the writer
            all.append(row)
        wr.writerows(all)
