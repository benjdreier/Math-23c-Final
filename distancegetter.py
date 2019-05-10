import requests
import csv

destination = "Los Angeles"
towns = set()

with open("MembershipRollsSmall.csv", "rU") as rolls:
    r = csv.reader(rolls)
    for row in r:
        print(row)
        towns.add(row[5])

print(len(towns), "uniqe towns")

hometown_dict = {}
for hometown in towns:
    response = requests.get("https://www.distance24.org/route.json?stops="+hometown+"|"+destination)
    data = response.json()
    dist = data["distance"]
    if dist == 0:
        print("Invalid town: ", hometown)
    hometown_dict[hometown] = dist

with open("MembershipRollsSmall.csv", "w") as rolls:
    with open("output.csv", "w") as output:
        r = csv.reader(rolls)
        wr = csv.writer(output)
        
        all = []
        header = r.next()
        header.append("Distance")
        all.append(header)
        print(header)
        for row in r:
            town_name = r[5]
            row.append(hometown_dict[town_name])
            all.append(row)
        writer.writerows(all)
