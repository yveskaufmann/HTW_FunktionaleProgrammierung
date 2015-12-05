L = [["Zulu","Rex","nil"],
["Zulu","Rex","Lin"],
["Rex","Zulu","nil"],
["Rex","Zulu","Lin"],
["Uzi","Rex","null"],
["Rex","Uzi","null"],
["Zulu","nil","Rex"],
["Zulu","Lin","Rex"],
["Uzi","null","Rex"],
["null","Uzi","Rex"],
["nil","Zulu","Rex"],
["Lin","Zulu","Rex"],
["rulez","Linux"],
["Rex","null","Uzi"],
["null","Rex","Uzi"],
["Linux","rulez"],
["Rex","nil","Zulu"],
["Rex","Lin","Zulu"],
["nil","Rex","Zulu"],
["Lin","Rex","Zulu"]];


L.reduce(function(list, x) {
	return list.concat(x);
}, [])
.reduce(function(list, e) {
	if (list.indexOf(e) == -1)
  		list.push(e);
	return list;
}, [])

function len(x) {
	if (typeof x.length === "number") {
		return x.length;
	}
	return 1;
}

function groupBy(groupBuilder, list) {
	return list.reduce(function(groups, element) {
		var groupKey = groupBuilder(element);
		groups[groupKey] = groups[groupKey] ? groups[groupKey] : [];
		groups[groupKey].push(element);
		return groups;
	}, {});
}



R = ["Zulu", "Rex", "nil", "Lin", "Uzi", "null", "rulez", "Linux"];

{
  "Zulu": 12,
  "Rex": 18,
  "nil": 6,
  "Lin": 6,
  "Uzi": 6,
  "null": 6,
  "rulez": 2,
  "Linux": 2
}