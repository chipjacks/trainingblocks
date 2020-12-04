FactoryBot.define do
  factory :run do
    duration { 30 }
    pace { "Easy" }
    completed { true }
    activity
  end
end
