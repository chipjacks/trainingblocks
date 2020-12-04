require 'rails_helper'

RSpec.describe Run, type: :model do
  subject do
    create(:run)
  end

  it "has a description, date, duration, pace, and completed" do
    expect(subject.activity.description).to be_truthy
    expect(subject.activity.date).to be_truthy
    expect(subject.duration).to be_truthy
    expect(subject.pace).to be_truthy
    expect(subject.completed).to be_truthy
  end
end
