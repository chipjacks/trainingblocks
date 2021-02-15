# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)

def migrate_data(data)
  type = data['type']
  duration = data['duration']

  if data['type'] == 'session'
    data['type'] = 'run'
    data['completed'] = true
  elsif data['type'] == 'run'
    data['effort'] = 'Easy'
  elsif data['type'] == 'interval'
    data['type'] = 'run'
    data['effort'] = 'Moderate'
  elsif data['type'] == 'race'
    data['type'] = 'run'
    data['effort'] = 'Hard'
    data['race'] = data['distance']
  elsif data['type'] == 'other' || data['type'] == 'note'
    data['type'] = 'other'
    data['completed'] = data['completed'] ? data['completed'] : true
  end

  data['type'] = data['type'].capitalize

  data
end

def create_activity(obj, me)
  data = obj['data']
  if data['type'] == 'session'
    data['laps'] = data['activities'].map do |a|
      migrate_data(a)
    end
    data.delete('activities')
  end

  obj['data'] = migrate_data(data)

  created = Activity.create(
    id: obj['id'],
    date: Date.parse(obj['date']),
    order: obj['order'],
    description: obj['description'],
    data: obj['data'],
    user: me
  )

end

me = User.find_by(uid: '2456610')

unless me then raise 'Strava uid not found.' end

me.activities.each{ |a| a.destroy! }

activities = JSON.parse(%x(cat db/activities.json | jq -cr '.').chomp)
last_date = nil
activities.each do |obj|
  create_activity(obj, me)
end

puts 'Success!'
