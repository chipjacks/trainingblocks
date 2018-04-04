require 'rails_helper'

xdescribe "User registration", type: :feature do

  scenario 'Is on homepage and wants to sign up' do
    visit root_path
    click_link "Sign up"
    expect(find('h2')).to have_content("Sign up")
  end

  scenario "User creates a new account" do
    visit new_user_registration_path

    fill_in "user[email]", :with => "jimbo@example.com"
    fill_in "user[password]", :with => "password"
    fill_in "user[password_confirmation]", :with => "password"
    click_button "Sign up"

    expect(page).to have_content("You have signed up successfully.")
  end

  scenario "User doesn't enter an email" do
    visit new_user_registration_path

    fill_in "user[email]", :with => ""
    fill_in "user[password]", :with => "password"
    fill_in "user[password_confirmation]", :with => "password"
    click_button "Sign up"

    expect(page).to have_content("Email can't be blank")
  end

  context "Is already signed in" do
    let(:user) { FactoryBot.create(:user) }

    before :each do
      sign_in user
    end
    scenario "User updates email address" do
      visit edit_user_registration_path

      fill_in "user[email]", :with => "example2@example.com"
      fill_in "user[current_password]", :with => user.password
      click_button "Update"

      expect(page).to have_content("Your account has been updated successfully.")
    end
  end
end