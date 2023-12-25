FactoryBot.define do
  factory :activity do
    id { '1234567890' }
    description { 'Friday jog' }
    data do
      {
        laps: [
          {
            type: 'Run',
            duration: 1800,
            pace: 455,
            effort: 'Easy',
            completed: true
          }
        ]
      }
    end
    user
  end
end
