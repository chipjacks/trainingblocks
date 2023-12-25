require 'rails_helper'

RSpec.describe Setting, type: :model do
  subject { create(:setting) }

  it 'has a list of paces' do
    expect(subject.paces[0].keys).to eq(%w[pace name])
  end
end
