require 'rails_helper'

RSpec.describe Activity, type: :model do
  subject do
    create(:activity)
  end

  it "has a description, date, and user" do
    expect(subject.description).to be_truthy
    expect(subject.date).to be_truthy
    expect(subject.user).to be_truthy
  end
end
