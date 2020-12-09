# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)

me = User.find_by(uid: "2456610")

unless me then raise "Strava uid not found." end

me.activities = %x(cat db/activities.json | jq -cr '.').chomp
me.save!
puts "Success!"
