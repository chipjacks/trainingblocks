require 'rails_helper'

RSpec.describe Activity, type: :model do
  subject do
    create(:activity)
  end

  it "has a description, user, and data" do
    expect(subject.description).to be_truthy
    expect(subject.user).to be_truthy
    expect(subject.data).to be_truthy
  end

  describe "#list" do
    before :each do
      @user = create(:user, :activities)
    end

    it "adds dates to activities" do
      result = Activity.list(@user)
      expect(result.first["date"]).to be_truthy
    end

    it "orders them chronologically" do
      result = Activity.list(@user)
      expect(result.map { |r| r["date"] }).to eq(result.map{ |r| r["date"] }.sort)
    end
  end
end
