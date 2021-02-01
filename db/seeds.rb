# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)

def trainingPaceToSeconds(tp)
  case tp
  when 'Easy'
    7 * 60 + 35
  when 'Moderate'
    6 * 60 + 44
  when 'Steady'
    6 * 60 + 29
  when 'Brisk'
    6 * 60 + 13
  when 'Aerobic'
    5 * 60 + 58
  when 'Lactate'
    5 * 60 + 43
  when 'Groove'
    5 * 60 + 27
  when 'VO2'
    5 * 60 + 12
  when 'Fast'
    4 * 60 + 56
  else
    7 * 60 + 45
  end
end

def migrate_data(data)
  type = data["type"]
  duration = data["duration"]
  if type != "interval" && duration
    # convert minutes to seconds
    data["duration"] = duration * 60
  end
  if data['pace']
    data['pace'] = trainingPaceToSeconds(data['pace'])
  end
  data
end

def create_activity(obj, order, me)
  obj['data'] = migrate_data(obj['data'])

  data = obj['data']
  if data["type"] == "session"
    data['activities'] = data["activities"].map do |a|
      migrate_data(a['data'])
    end
    return
  elsif data["type"] == "note"
    return
  end

  created = Activity.create(
    id: obj["id"].length == 10 ? obj["id"] : Array.new(10) { |i| rand(10) }.join,
    date: Date.parse(obj["date"]),
    order: order,
    description: obj["description"],
    data: obj["data"],
    user: me
  )

end

me = User.find_by(uid: "2456610")

unless me then raise "Strava uid not found." end

me.activities.each{ |a| a.destroy! }

activities = JSON.parse(%x(cat db/activities.json | jq -cr '.').chomp)
last_date = nil
order = 0
activities.each do |obj|
  create_activity(obj, order, me)

  if obj['date'] == last_date
    order += 1
  else
    order = 0
    last_date = obj['date']
  end
end

puts "Success!"
