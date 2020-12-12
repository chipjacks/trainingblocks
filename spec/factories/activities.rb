FactoryBot.define do
  factory :activity do
    id { "1234567890" }
    description { "Friday jog" }
    data { { type: "run", duration: 30, pace: "Easy", completed: true } }
    user
  end
end
