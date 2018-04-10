require 'rails_helper'

RSpec.describe ActivitiesController, type: :controller do
  let(:user) { FactoryBot.create(:user) }

  before do
    sign_in user
  end

  describe "GET #index" do
    before :each do
      6.times { FactoryBot.create(:activity, user: user) }
    end

    it "lists activities" do
      get :index, format: :json
      expect(JSON.parse(response.body).size).to eq 6
    end
  end

  describe "POST #create" do
    it "creates a new activity with all the attributes" do
      activity = FactoryBot.build(:activity, user: user)
      response = post :create, params: { activity: JSON.parse(activity.to_json) }, format: :json
      new_activity = JSON.parse(response.body)
      expect(new_activity.slice(*%w(start_date duration distance completed external_id user_id id)).values).to_not include nil
    end
  end

  describe "PUT #update" do
    it "updates activity details" do
      activity = FactoryBot.create(:activity, user: user)
      expect {
        put :update, params: { id: activity.id,
          activity: {duration: "300"}
        }, format: :json
        activity.reload
      }.to change { activity.duration }
    end
  end

  describe "DELETE #destroy" do
    it "destroys an appointment" do
      activity = FactoryBot.create(:activity, user: user)
      expect {
        delete :destroy, params: {id: activity.id}, format: :json
      }.to change { user.activities.count }.by (-1)
    end
  end

end
