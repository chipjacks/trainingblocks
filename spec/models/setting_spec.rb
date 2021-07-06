require 'rails_helper'

RSpec.describe Setting, type: :model do
  subject do
    create(:setting)
  end

  it "has a list of paces" do
    expect(subject.paces[0].keys).to eq(%w(pace name))
    binding.pry
  end
end