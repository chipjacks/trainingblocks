FactoryBot.define do
  factory :activity do
    date { "2020-12-04" }
    description { "Friday jog" }
    user
  end
end
