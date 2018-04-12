require 'rails_helper'

RSpec.describe ActivitiesController, type: :controller do
  let(:user) { FactoryBot.create(:user) }

  before do
    sign_in user
  end

  describe "GET #index" do
    before :each do
      @start_date = DateTime.parse("2018-04-02 11:07:12")
      6.times do |i|
        FactoryBot.create(:activity, user: user, start_date: @start_date - i)
      end
      allow(subject).to receive(:fetch_external_activities).and_return(
        JSON.parse(external_fixture('strava_activities.json')).map {|a|
          StravaClient::SummaryActivity.new(a)
        }
      )
    end

    it "merges external activities" do
      get :index, format: :json, params: {before: @start_date.to_i}
      expect(JSON.parse(response.body).size).to be > 6
    end
  end

  describe "POST #create" do
    it "creates a new activity with all the attributes" do
      activity = FactoryBot.build(:activity, user: user)
      response = post :create, params: { activity: JSON.parse(activity.to_json) }, format: :json
      new_activity = JSON.parse(response.body)
      expect(new_activity.slice(*%w(start_date type_ name duration distance completed external_id user_id id)).values).to_not include nil
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
