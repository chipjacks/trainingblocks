FactoryBot.define do
  factory :activity do
    user
    start_date "2018-04-09 11:07:12"
    duration 240
    distance 1609.34
    completed false
    external_id "MyString"
  end
end
