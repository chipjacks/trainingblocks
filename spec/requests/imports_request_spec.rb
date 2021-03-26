require 'rails_helper'

RSpec.describe "Imports", type: :request do
  before :each do
    @user = create(:user, :strava)
  end

  describe "GET #strava_push" do
    it "echoes challenge" do
      get "/imports/strava_push", params: {"hub.challenge"=>"3a35806f017de930", "hub.mode"=>"subscribe", "hub.verify_token"=>"STRAVA"}

      expect(response).to have_http_status(200)
      expect(JSON.parse(response.body)).to eq({"hub.challenge"=>"3a35806f017de930"})
    end
  end

  describe "POST #strava_push" do
    before :each do
      @import = create(:import, user: @user)
    end

    it "marks updated imports stale" do
      post "/imports/strava_push", params:
        { "aspect_type"=>"update",
          "event_time"=>1616713412,
          "object_id"=>@import.id,
          "object_type"=>"activity",
          "owner_id"=>@user.uid,
          "subscription_id"=>186832
        }
      
      expect(response).to have_http_status(200)
      expect(@import.reload.stale).to be(true)
    end

    it "creates new imports" do
      id = 12345678
      post "/imports/strava_push", params:
        { "aspect_type"=>"create",
          "event_time"=>1616713412,
          "object_id"=> id,
          "object_type"=>"activity",
          "owner_id"=>@user.uid,
          "subscription_id"=>186832
        }
      
      expect(response).to have_http_status(200)
      expect(Import.find(id).stale).to be(true)
      expect(Import.find(id).data).to eq({})
    end

    it "deletes imports" do
      post "/imports/strava_push", params:
        { "aspect_type"=>"delete",
          "event_time"=>1616713412,
          "object_id"=>@import.id,
          "object_type"=>"activity",
          "owner_id"=>@user.uid,
          "subscription_id"=>186832
        }
      
      expect(response).to have_http_status(200)
      expect { @import.reload }.to raise_error(ActiveRecord::RecordNotFound)
    end
  end
end
